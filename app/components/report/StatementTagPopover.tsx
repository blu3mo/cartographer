"use client";

import axios from "axios";
import { Loader2 } from "lucide-react";
import { useCallback, useEffect, useRef, useState } from "react";

interface StatementResponse {
  participantName: string;
  responseType: "scale" | "free_text";
  value: number | null;
  valueLabel: string | null;
  textResponse: string | null;
}

interface StatementData {
  statement: {
    id: string;
    number: number;
    text: string;
  };
  responses: StatementResponse[];
}

interface StatementTagPopoverProps {
  statementNumber: number;
  sessionId: string;
  accessToken: string;
  reportId: string;
  userId: string;
}

// Simple cache to avoid refetching on every hover
const dataCache = new Map<string, StatementData>();

export function StatementTagPopover({
  statementNumber,
  sessionId,
  accessToken,
  reportId,
  userId,
}: StatementTagPopoverProps) {
  const [isOpen, setIsOpen] = useState(false);
  const [data, setData] = useState<StatementData | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const timeoutRef = useRef<NodeJS.Timeout | null>(null);
  const containerRef = useRef<HTMLSpanElement>(null);

  const cacheKey = `${sessionId}-${statementNumber}`;

  const fetchData = useCallback(async () => {
    const cached = dataCache.get(cacheKey);
    if (cached) {
      setData(cached);
      return;
    }

    setLoading(true);
    setError(null);

    try {
      const response = await axios.get(
        `/api/sessions/${sessionId}/${accessToken}/reports/${reportId}/statement-responses`,
        {
          params: { statementNumber },
          headers: { Authorization: `Bearer ${userId}` },
        },
      );
      const result = response.data as StatementData;
      dataCache.set(cacheKey, result);
      setData(result);
    } catch (err) {
      console.error("Failed to fetch statement responses:", err);
      setError("データの取得に失敗しました");
    } finally {
      setLoading(false);
    }
  }, [sessionId, accessToken, reportId, statementNumber, userId, cacheKey]);

  const handleMouseEnter = () => {
    if (timeoutRef.current) {
      clearTimeout(timeoutRef.current);
    }
    // Small delay before opening to avoid accidental triggers
    timeoutRef.current = setTimeout(() => {
      setIsOpen(true);
      if (!data && !loading) {
        void fetchData();
      }
    }, 150);
  };

  const handleMouseLeave = () => {
    if (timeoutRef.current) {
      clearTimeout(timeoutRef.current);
    }
    // Delay before closing to allow moving to popover
    timeoutRef.current = setTimeout(() => {
      setIsOpen(false);
    }, 100);
  };

  useEffect(() => {
    return () => {
      if (timeoutRef.current) {
        clearTimeout(timeoutRef.current);
      }
    };
  }, []);

  const getValueBgColor = (value: number | null) => {
    if (value === null) return "bg-muted";
    if (value >= 1) return "bg-emerald-100 text-emerald-800";
    if (value <= -1) return "bg-rose-100 text-rose-800";
    return "bg-amber-100 text-amber-800";
  };

  return (
    <span
      ref={containerRef}
      className="relative inline-block"
      onMouseEnter={handleMouseEnter}
      onMouseLeave={handleMouseLeave}
    >
      <span className="cursor-help rounded bg-muted px-1 py-0.5 font-mono text-sm text-slate-700 transition-colors hover:bg-slate-200">
        #{statementNumber}
      </span>

      {isOpen && (
        <span
          className="absolute left-1/2 z-50 mt-2 block w-80 -translate-x-1/2 transform"
          onMouseEnter={handleMouseEnter}
          onMouseLeave={handleMouseLeave}
        >
          <span className="block rounded-lg border border-border bg-white p-4 shadow-lg">
            {loading && (
              <span className="flex items-center justify-center py-4">
                <Loader2 className="h-5 w-5 animate-spin text-slate-400" />
              </span>
            )}

            {error && (
              <span className="block text-center text-sm text-rose-600">
                {error}
              </span>
            )}

            {data && !loading && (
              <span className="block space-y-3">
                <span className="block border-b border-slate-100 pb-2">
                  <span className="block text-[10px] font-semibold uppercase tracking-wider text-slate-400">
                    問い #{data.statement.number}
                  </span>
                  <span className="mt-1 block text-sm font-medium text-slate-800">
                    {data.statement.text}
                  </span>
                </span>

                {data.responses.length === 0 ? (
                  <span className="block text-center text-xs text-slate-500">
                    まだ回答がありません
                  </span>
                ) : (
                  <span className="block max-h-48 space-y-2 overflow-y-auto">
                    {data.responses.map((response, index) => (
                      <span
                        key={index}
                        className="flex items-start gap-2 text-sm"
                      >
                        <span className="shrink-0 font-medium text-slate-700">
                          {response.participantName}:
                        </span>
                        {response.responseType === "scale" ? (
                          <span
                            className={`rounded px-1.5 py-0.5 text-xs font-medium ${getValueBgColor(response.value)}`}
                          >
                            {response.valueLabel}
                          </span>
                        ) : (
                          <span className="rounded bg-muted px-2 py-1 text-xs font-normal text-slate-700">
                            {response.textResponse}
                          </span>
                        )}
                      </span>
                    ))}
                  </span>
                )}
              </span>
            )}
          </span>

          {/* Arrow */}
          <span className="absolute -top-2 left-1/2 block h-0 w-0 -translate-x-1/2 transform border-x-8 border-b-8 border-x-transparent border-b-white drop-shadow-sm" />
        </span>
      )}
    </span>
  );
}
