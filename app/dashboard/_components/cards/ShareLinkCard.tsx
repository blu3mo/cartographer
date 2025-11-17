"use client";

import { ChevronDown, ChevronUp, Copy, ExternalLink, Maximize2 } from "lucide-react";
import Image from "next/image";

import { Button } from "@/components/ui/Button";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";

const SHARE_QR_SIZE = 176;

const buildQrUrl = (url: string, size: number) =>
  `https://api.qrserver.com/v1/create-qr-code/?size=${size}x${size}&data=${encodeURIComponent(
    url,
  )}`;

interface ShareLinkCardProps {
  shareLink: string;
  isCollapsed: boolean;
  onToggleCollapsed: () => void;
  copyStatus: "idle" | "copied" | "error";
  onCopyLink: () => Promise<void>;
  onOpenQrFullscreen: () => void;
  isShareQrErrored: boolean;
  onQrError: () => void;
  onQrLoad: () => void;
}

export function ShareLinkCard({
  shareLink,
  isCollapsed,
  onToggleCollapsed,
  copyStatus,
  onCopyLink,
  onOpenQrFullscreen,
  isShareQrErrored,
  onQrError,
  onQrLoad,
}: ShareLinkCardProps) {
  const shareQrSrc = shareLink ? buildQrUrl(shareLink, SHARE_QR_SIZE) : "";

  return (
    <Card>
      <CardHeader className="pb-2">
        <div className="flex items-start justify-between gap-2">
          <div>
            <CardTitle className="text-sm font-semibold">
              参加用リンク
            </CardTitle>
            <CardDescription className="text-xs">
              招待URLとQRコードをまとめて共有できます。
            </CardDescription>
          </div>
          <button
            type="button"
            onClick={onToggleCollapsed}
            className="text-muted-foreground transition hover:text-foreground"
            aria-expanded={!isCollapsed}
            aria-controls="aside-shareLink"
          >
            {isCollapsed ? (
              <ChevronDown className="h-4 w-4" />
            ) : (
              <ChevronUp className="h-4 w-4" />
            )}
            <span className="sr-only">セクションを切り替え</span>
          </button>
        </div>
      </CardHeader>
      {!isCollapsed && (
        <CardContent id="aside-shareLink" className="space-y-3">
          {shareLink ? (
            <>
              <div className="rounded-md border border-border/70 bg-muted/40 px-3 py-2 text-xs text-muted-foreground">
                <span className="break-all">{shareLink}</span>
              </div>
              <div className="flex flex-wrap gap-2">
                <Button
                  variant="outline"
                  size="sm"
                  onClick={() => {
                    void onCopyLink();
                  }}
                >
                  <Copy className="h-3.5 w-3.5" />
                  {copyStatus === "copied"
                    ? "コピーしました"
                    : copyStatus === "error"
                      ? "コピーできません"
                      : "リンクをコピー"}
                </Button>
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={() =>
                    window.open(shareLink, "_blank", "noreferrer")
                  }
                >
                  <ExternalLink className="h-3.5 w-3.5" />
                  新しいタブで開く
                </Button>
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={onOpenQrFullscreen}
                  disabled={!shareQrSrc || isShareQrErrored}
                >
                  <Maximize2 className="h-3.5 w-3.5" />
                  QRを拡大
                </Button>
              </div>
              {shareQrSrc && !isShareQrErrored ? (
                <div className="flex justify-center">
                  <Image
                    src={shareQrSrc}
                    alt="参加用QRコード"
                    width={SHARE_QR_SIZE}
                    height={SHARE_QR_SIZE}
                    className="rounded-lg border border-border/60 bg-white p-2"
                    onError={onQrError}
                    onLoadingComplete={onQrLoad}
                  />
                </div>
              ) : shareQrSrc ? (
                <p className="text-[11px] text-muted-foreground">
                  QRコードを生成できませんでした。
                </p>
              ) : null}
            </>
          ) : (
            <p className="text-xs text-muted-foreground">
              管理権限のあるセッションを選択すると、共有リンクが表示されます。
            </p>
          )}
        </CardContent>
      )}
    </Card>
  );
}
