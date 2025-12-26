"use client";

import { Check, Copy, Mic, MicOff } from "lucide-react";
import { useCallback, useEffect, useRef, useState } from "react";

interface TranscriptSegment {
  id: string;
  text: string;
  timestamp: Date;
  isFinal: boolean;
}

export function RealtimeTranscription() {
  const [isRecording, setIsRecording] = useState(false);
  const [transcripts, setTranscripts] = useState<TranscriptSegment[]>([]);
  const [currentPartial, setCurrentPartial] = useState<string>("");
  const [copiedId, setCopiedId] = useState<string | null>(null);
  const [copiedAll, setCopiedAll] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const wsRef = useRef<WebSocket | null>(null);
  const audioContextRef = useRef<AudioContext | null>(null);
  const streamRef = useRef<MediaStream | null>(null);
  const transcriptEndRef = useRef<HTMLDivElement>(null);

  // Auto-scroll to bottom when new transcripts arrive
  useEffect(() => {
    transcriptEndRef.current?.scrollIntoView({ behavior: "smooth" });
  }, []);

  const stopRecording = useCallback(() => {
    // Close WebSocket
    if (wsRef.current) {
      wsRef.current.close();
      wsRef.current = null;
    }

    // Stop audio context
    if (audioContextRef.current) {
      audioContextRef.current.close();
      audioContextRef.current = null;
    }

    // Stop media stream
    if (streamRef.current) {
      streamRef.current.getTracks().forEach((track) => {
        track.stop();
      });
      streamRef.current = null;
    }

    setIsRecording(false);
  }, []);

  const startRecording = useCallback(async () => {
    try {
      setError(null);

      // Get single-use token from our API
      const tokenResponse = await fetch("/api/transcription/token", {
        method: "POST",
      });

      if (!tokenResponse.ok) {
        throw new Error("Failed to get transcription token");
      }

      const { token } = await tokenResponse.json();

      // Request microphone access
      const stream = await navigator.mediaDevices.getUserMedia({
        audio: {
          channelCount: 1,
          sampleRate: 16000,
          echoCancellation: true,
          noiseSuppression: true,
        },
      });

      streamRef.current = stream;

      // Create WebSocket connection to ElevenLabs
      const ws = new WebSocket(
        `wss://api.elevenlabs.io/v1/speech-to-text/realtime?model_id=scribe_v2_realtime&token=${token}&include_timestamps=false&commit_strategy=vad`,
      );

      wsRef.current = ws;

      ws.onopen = () => {
        console.log("WebSocket connected");
        setIsRecording(true);
      };

      ws.onmessage = (event) => {
        try {
          const data = JSON.parse(event.data);

          if (data.message_type === "session_started") {
            console.log("Session started:", data);
          } else if (data.message_type === "partial_transcript") {
            // Update partial transcript (temporary, not yet final)
            setCurrentPartial(data.text || "");
          } else if (data.message_type === "committed_transcript") {
            // Add final transcript segment
            const segment: TranscriptSegment = {
              id: `${Date.now()}-${Math.random()}`,
              text: data.text || "",
              timestamp: new Date(),
              isFinal: true,
            };
            setTranscripts((prev) => [...prev, segment]);
            setCurrentPartial(""); // Clear partial when committed
          } else if (data.message_type === "error") {
            console.error("Transcription error:", data);
            setError(data.message || "Transcription error occurred");
          }
        } catch (err) {
          console.error("Error parsing WebSocket message:", err);
        }
      };

      ws.onerror = (err) => {
        console.error("WebSocket error:", err);
        setError("Connection error occurred");
      };

      ws.onclose = () => {
        console.log("WebSocket closed");
        setIsRecording(false);
      };

      // Set up audio processing
      const audioContext = new AudioContext({ sampleRate: 16000 });
      audioContextRef.current = audioContext;

      const source = audioContext.createMediaStreamSource(stream);
      const processor = audioContext.createScriptProcessor(4096, 1, 1);

      processor.onaudioprocess = (e) => {
        if (ws.readyState === WebSocket.OPEN) {
          const inputData = e.inputBuffer.getChannelData(0);

          // Convert float32 to int16 PCM
          const pcm16 = new Int16Array(inputData.length);
          for (let i = 0; i < inputData.length; i++) {
            const s = Math.max(-1, Math.min(1, inputData[i]));
            pcm16[i] = s < 0 ? s * 0x8000 : s * 0x7fff;
          }

          // Convert to base64
          const base64 = btoa(
            String.fromCharCode(...new Uint8Array(pcm16.buffer)),
          );

          // Send audio chunk to ElevenLabs
          ws.send(
            JSON.stringify({
              message_type: "input_audio_chunk",
              audio_base_64: base64,
              sample_rate: 16000,
              commit: false, // VAD will auto-commit
            }),
          );
        }
      };

      source.connect(processor);
      processor.connect(audioContext.destination);
    } catch (err) {
      console.error("Error starting recording:", err);
      const errorMessage =
        err instanceof Error
          ? `${err.name}: ${err.message}`
          : "Failed to start recording";
      console.error("Detailed error:", errorMessage);
      setError(errorMessage);
      stopRecording();
    }
  }, [stopRecording]);

  const copyToClipboard = useCallback(async (text: string, id: string) => {
    try {
      await navigator.clipboard.writeText(text);
      setCopiedId(id);
      setTimeout(() => setCopiedId(null), 2000);
    } catch (err) {
      console.error("Failed to copy:", err);
    }
  }, []);

  const clearTranscripts = useCallback(() => {
    setTranscripts([]);
    setCurrentPartial("");
  }, []);

  const copyAllTranscripts = useCallback(async () => {
    try {
      const fullText = transcripts
        .map((segment) => {
          const timeLabel = segment.timestamp.toLocaleTimeString("ja-JP");
          return `[${timeLabel}] ${segment.text}`;
        })
        .join("\n\n");

      await navigator.clipboard.writeText(fullText);
      setCopiedAll(true);
      setTimeout(() => setCopiedAll(false), 2000);
    } catch (err) {
      console.error("Failed to copy all transcripts:", err);
    }
  }, [transcripts]);

  return (
    <div className="space-y-4">
      {/* Control buttons */}
      <div className="flex items-center gap-3">
        <button
          type="button"
          onClick={isRecording ? stopRecording : startRecording}
          className={`flex items-center gap-2 rounded-xl px-6 py-3 font-medium text-white shadow-lg transition-all ${
            isRecording
              ? "bg-red-500 hover:bg-red-600"
              : "bg-emerald-500 hover:bg-emerald-600"
          }`}
        >
          {isRecording ? (
            <>
              <MicOff className="h-5 w-5" />
              <span>停止</span>
            </>
          ) : (
            <>
              <Mic className="h-5 w-5" />
              <span>文字起こし開始</span>
            </>
          )}
        </button>

        {transcripts.length > 0 && (
          <>
            <button
              type="button"
              onClick={copyAllTranscripts}
              className="flex items-center gap-2 rounded-xl border border-slate-200 bg-white px-4 py-3 text-sm font-medium text-slate-700 shadow-sm hover:bg-slate-50"
            >
              {copiedAll ? (
                <>
                  <Check className="h-4 w-4 text-emerald-500" />
                  <span>コピー済み</span>
                </>
              ) : (
                <>
                  <Copy className="h-4 w-4" />
                  <span>全文コピー</span>
                </>
              )}
            </button>
            <button
              type="button"
              onClick={clearTranscripts}
              className="rounded-xl border border-slate-200 bg-white px-4 py-3 text-sm font-medium text-slate-700 shadow-sm hover:bg-slate-50"
            >
              クリア
            </button>
          </>
        )}
      </div>

      {/* Recording indicator */}
      {isRecording && (
        <div className="flex items-center gap-2 rounded-xl border border-red-200 bg-red-50/50 px-4 py-3">
          <div className="flex h-3 w-3 items-center justify-center">
            <span className="absolute inline-flex h-3 w-3 animate-ping rounded-full bg-red-400 opacity-75"></span>
            <span className="relative inline-flex h-2 w-2 rounded-full bg-red-500"></span>
          </div>
          <span className="text-sm font-medium text-red-900">録音中...</span>
        </div>
      )}

      {/* Error display */}
      {error && (
        <div className="rounded-xl border border-red-200 bg-red-50 px-4 py-3">
          <p className="text-sm text-red-900">{error}</p>
        </div>
      )}

      {/* Transcription timeline */}
      <div className="rounded-xl border border-slate-200 bg-white shadow-sm">
        <div className="border-b border-slate-200 px-4 py-3">
          <p className="text-xs font-medium uppercase tracking-wider text-slate-500">
            文字起こし
          </p>
        </div>

        <div className="max-h-96 overflow-y-auto p-4">
          {transcripts.length === 0 && !currentPartial && !isRecording && (
            <div className="py-8 text-center">
              <p className="text-sm text-slate-500">
                マイクボタンをクリックして文字起こしを開始してください
              </p>
            </div>
          )}

          <div className="space-y-3">
            {/* Final transcripts */}
            {transcripts.map((segment) => (
              <div
                key={segment.id}
                className="group relative rounded-lg border border-slate-200 bg-slate-50/50 p-3 hover:bg-slate-100/50"
              >
                <div className="flex items-start justify-between gap-3">
                  <div className="flex-1">
                    <p className="text-xs text-slate-500">
                      {segment.timestamp.toLocaleTimeString("ja-JP")}
                    </p>
                    <p className="mt-1 text-sm leading-relaxed text-slate-900">
                      {segment.text}
                    </p>
                  </div>
                  <button
                    type="button"
                    onClick={() => copyToClipboard(segment.text, segment.id)}
                    className="flex-shrink-0 rounded-lg p-2 text-slate-400 opacity-0 transition-opacity hover:bg-white hover:text-slate-600 group-hover:opacity-100"
                    title="コピー"
                  >
                    {copiedId === segment.id ? (
                      <Check className="h-4 w-4 text-emerald-500" />
                    ) : (
                      <Copy className="h-4 w-4" />
                    )}
                  </button>
                </div>
              </div>
            ))}

            {/* Partial (in-progress) transcript */}
            {currentPartial && (
              <div className="rounded-lg border border-blue-200 bg-blue-50/50 p-3">
                <p className="text-xs text-blue-600">認識中...</p>
                <p className="mt-1 text-sm leading-relaxed text-blue-900 opacity-75">
                  {currentPartial}
                </p>
              </div>
            )}

            <div ref={transcriptEndRef} />
          </div>
        </div>
      </div>

      {/* Helper text */}
      {transcripts.length > 0 && (
        <p className="text-xs text-slate-500">
          各セグメントをクリックしてコピーし、会議のコンテキストとして活用できます
        </p>
      )}
    </div>
  );
}
