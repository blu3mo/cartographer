"use client";

import Image from "next/image";
import { Copy, Check } from "lucide-react";
import { Input } from "@/components/ui/input";
import { useState } from "react";
import { Button } from "@/components/ui/Button";


export const URLShareCard = ({
	shareUrl,
	alt,
}: {
	shareUrl: string | null;
	alt: string;
}) => {
	const SHARE_QR_SIZE = 176;
	const shareQrUrl = shareUrl
		? `https://api.qrserver.com/v1/create-qr-code/?size=${SHARE_QR_SIZE}x${SHARE_QR_SIZE}&data=${encodeURIComponent(
			shareUrl,
		)}`
		: null;
	const [copyStatus, setCopyStatus] = useState<"idle" | "copied" | "error">(
		"idle",
	);

	const handleCopyLink = async () => {
		if (!shareUrl) return;
		try {
			await navigator.clipboard.writeText(shareUrl);
			setCopyStatus("copied");
			window.setTimeout(() => setCopyStatus("idle"), 2000);
		} catch (err) {
			console.error("Failed to copy link:", err);
			setCopyStatus("error");
			window.setTimeout(() => setCopyStatus("idle"), 2000);
		}
	};

	return (

		<div className="relative rounded-2xl border border-slate-200/80 bg-gradient-to-br from-slate-50 to-white px-6 py-6 text-center shadow-inner items-center flex flex-col gap-4">
			{shareQrUrl ? (
				<Image
					src={shareQrUrl}
					alt={alt}
					width={SHARE_QR_SIZE}
					height={SHARE_QR_SIZE}
					className="mx-auto h-[176px] w-[176px] rounded-xl border border-slate-200 bg-white object-contain p-2 shadow-sm"
				/>
			) : (
				<div className="mx-auto flex h-[176px] w-[176px] items-center justify-center rounded-xl border border-dashed border-slate-300 bg-white text-xs text-slate-400">
					QRコードを生成できませんでした
				</div>
			)}
			{shareUrl ? (

				<div className="flex items-center gap-2">
					<Input
						id="shareLink"
						readOnly
						value={shareUrl}
						className="text-sm"
						onFocus={(event) => event.currentTarget.select()}
					/>
					<Button
						type="button"
						variant="outline"
						size="sm"
						onClick={handleCopyLink}
						className="gap-1.5 text-xs"
					>
						{copyStatus === "copied" ? (
							<Check className="h-3.5 w-3.5 text-emerald-600" />
						) : (
							<Copy className="h-3.5 w-3.5" />
						)}
						{copyStatus === "copied"
							? "コピー済み"
							: copyStatus === "error"
								? "コピー失敗"
								: "コピー"}
					</Button>
				</div>) : (<div>コピー用URLを生成できませんでした</div>)}
		</div>
	);
}
