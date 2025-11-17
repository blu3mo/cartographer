"use client";

import type { FC } from "react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import { Copy, FileText, Loader2, Share } from "lucide-react";

import { Button } from "@/components/ui/Button";
import {
	Card,
	CardContent,
	CardDescription,
	CardHeader,
	CardTitle,
} from "@/components/ui/card";

import type { SessionReport, SessionReportStatus } from "./page";

type SessionReportProps = {
	reports: SessionReport[];
	reportsLoading: boolean;
	reportsError: string | null;
	selectedReportId: string | null;
	setSelectedReportId: (id: string) => void;
	reportRequest: string;
	setReportRequest: (value: string) => void;
	creatingReport: boolean;
	reportCopyStatus: "idle" | "copied" | "error";
	handleCreateReport: (event: React.FormEvent<HTMLFormElement>) => void;
	handleCopyReportMarkdown: () => void;
	canEdit: boolean;
	selectedReport: SessionReport | null;
	isViewingLatestReport: boolean;
	formatDateTime: (value: string) => string;
	reportPrintPageURL: string;
};

const REPORT_STATUS_META: Record<
	SessionReportStatus,
	{ label: string; dot: string; text: string }
> = {
	pending: {
		label: "待機中",
		dot: "bg-amber-500",
		text: "text-amber-600",
	},
	generating: {
		label: "生成中",
		dot: "bg-sky-500",
		text: "text-sky-600",
	},
	completed: {
		label: "完了",
		dot: "bg-emerald-500",
		text: "text-emerald-600",
	},
	failed: {
		label: "失敗",
		dot: "bg-rose-500",
		text: "text-rose-600",
	},
};

export const SessionReportCard: FC<SessionReportProps> = ({
	reports,
	reportsLoading,
	reportsError,
	selectedReportId,
	setSelectedReportId,
	reportRequest,
	setReportRequest,
	creatingReport,
	reportCopyStatus,
	handleCreateReport,
	handleCopyReportMarkdown,
	canEdit,
	selectedReport,
	isViewingLatestReport,
	formatDateTime,
	reportPrintPageURL,
}) => {
	return (
		<Card className="border-none bg-white/80 shadow-sm">
			<CardHeader className="pb-4">
				<div className="flex flex-wrap items-start justify-between gap-4">
					<div className="space-y-1.5">
						<CardTitle className="text-lg">セッションレポート</CardTitle>
						<CardDescription>
							参加者の回答をもとに洞察レポートを生成します
						</CardDescription>
					</div>
				</div>
			</CardHeader>
			<CardContent className="space-y-6">
				{reportsError && (
					<div className="rounded-2xl border border-amber-200 bg-amber-50 px-4 py-3 text-sm text-amber-700">
						{reportsError}
					</div>
				)}

				{reportsLoading ? (
					<div className="space-y-4">
						<div className="h-12 animate-pulse rounded-2xl bg-slate-100/80" />
						<div className="h-[420px] animate-pulse rounded-3xl bg-slate-100/80" />
					</div>
				) : reports.length === 0 ? (
					<div className="rounded-2xl border border-dashed border-slate-200/80 bg-white/70 px-4 py-6 text-center text-sm text-slate-500">
						まだレポートはありません。上のフォームから生成してみましょう。
					</div>
				) : selectedReport ? (
					<div className="space-y-6">
						<div className="flex flex-col gap-4 lg:flex-row lg:items-center lg:justify-between">
							<div className="space-y-2">
								<label
									htmlFor="reportVersionSelect"
									className="text-xs font-semibold uppercase tracking-[0.18em] text-slate-500"
								>
									表示するバージョン
								</label>
								<div className="flex flex-wrap items-center gap-3">
									<select
										id="reportVersionSelect"
										value={selectedReportId ?? ""}
										onChange={(event) => setSelectedReportId(event.target.value)}
										className="max-w-xs rounded-2xl border border-slate-200 bg-white px-3 py-2 text-sm font-medium text-slate-800 shadow-sm focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-slate-200"
									>
										{reports.map((report) => {
											return (
												<option key={report.id} value={report.id}>
													v{String(report.version).padStart(2, "0")}(
													{isViewingLatestReport ? "最新バージョン" : ""})
												</option>
											);
										})}
									</select>
									<span className="inline-flex items-center gap-1 rounded-full border border-slate-200 px-3 py-1 text-[11px] font-medium text-slate-600">
										<span
											className={`h-2 w-2 rounded-full ${REPORT_STATUS_META[selectedReport.status].dot}`}
										/>
										{REPORT_STATUS_META[selectedReport.status].label}
									</span>
								</div>
							</div>
						</div>

						{selectedReport.requestMarkdown ? (
							<div className="rounded-2xl border border-indigo-100 bg-indigo-50/70 p-4 text-sm text-indigo-900">
								<p className="text-[11px] font-semibold uppercase tracking-[0.18em] text-indigo-400">
									Admin Request
								</p>
								<p className="mt-1 whitespace-pre-wrap leading-relaxed">
									{selectedReport.requestMarkdown}
								</p>
							</div>
						) : null}

						<div className="min-h-[360px] rounded-3xl border border-slate-200 bg-white/80 p-6 shadow-inner">
							<div className="flex h-full flex-col gap-4">
								<div className="space-y-2">
									<div className="flex flex-wrap justify-end gap-2">
										<Button
											type="button"
											variant="outline"
											size="sm"
											className="gap-1.5 text-xs"
											disabled={
												selectedReport.status !== "completed" ||
												!selectedReport.contentMarkdown
											}
											onClick={() => {
												if (!selectedReport) return;
												if (!reportPrintPageURL) return;
												window.open(reportPrintPageURL, "_blank");
											}}
										>
											<Share className="h-3.5 w-3.5" />
											共有用ページ
										</Button>
									</div>
								</div>

								<div className="flex flex-wrap items-center justify-between gap-3">
									<div>
										<p className="text-xl font-semibold text-slate-900">
											v{String(selectedReport.version).padStart(2, "0")}
										</p>
									</div>
									<div className="inline-flex items-center gap-2 text-[11px] uppercase tracking-[0.2em] text-slate-400">
										<div className="inline-flex items-center gap-1 rounded-full border border-slate-200 px-2 py-1 text-[10px] font-semibold uppercase tracking-[0.18em] text-slate-500">
											{selectedReport.model}
										</div>
										<div className="inline-flex items-center gap-1 rounded-full border border-slate-200 px-2 py-1 text-[10px] font-semibold uppercase tracking-[0.18em]">
											<span
												className={`h-2 w-2 rounded-full ${REPORT_STATUS_META[selectedReport.status].dot}`}
											/>
											{REPORT_STATUS_META[selectedReport.status].label}
										</div>
									</div>
								</div>

								<div className="flex-1 overflow-y-auto 1 rounded-2xl border border-slate-200 bg-white/90 p-4 flex flex-col gap-4">
									<div className="flex justify-end">
										<Button
											type="button"
											variant="ghost"
											size="sm"
											disabled={
												selectedReport.status !== "completed" ||
												!selectedReport.contentMarkdown
											}
											onClick={handleCopyReportMarkdown}
											className="gap-1.5 text-xs"
										>
											<Copy className="h-3.5 w-3.5" />
											{reportCopyStatus === "copied"
												? "コピー済み"
												: reportCopyStatus === "error"
													? "コピー失敗"
													: "Markdownをコピー"}
										</Button>
									</div>

									{selectedReport.status === "completed" &&
										selectedReport.contentMarkdown ? (
										<div className="markdown-body prose prose-slate max-w-none text-sm leading-relaxed">
											<ReactMarkdown remarkPlugins={[remarkGfm]}>
												{selectedReport.contentMarkdown}
											</ReactMarkdown>
										</div>
									) : selectedReport.status === "failed" ? (
										<div className="text-sm text-rose-600">
											レポート生成に失敗しました。
											<br />
											{selectedReport.errorMessage ??
												"詳細はログを確認してください。"}
										</div>
									) : (
										<div className="flex h-full flex-col items-center justify-center gap-3 text-sm text-slate-500">
											<Loader2 className="h-5 w-5 animate-spin text-slate-400" />
											<p>レポートを生成しています…</p>
											<p className="text-[11px] text-slate-400">
												完了まで数十秒ほどかかる場合があります。
											</p>
										</div>
									)}
								</div>

								<div className="flex flex-wrap gap-4 text-[11px] uppercase tracking-[0.2em] text-slate-400">
									<span>作成: {formatDateTime(selectedReport.createdAt)}</span>
									{selectedReport.completedAt ? (
										<span>
											最終更新: {formatDateTime(selectedReport.completedAt)}
										</span>
									) : null}
								</div>
							</div>
						</div>
					</div>
				) : (
					<div className="rounded-2xl border border-dashed border-slate-200/80 bg-white/70 px-4 py-6 text-center text-sm text-slate-500">
						レポートを選択するとここに表示されます。
					</div>
				)}

				{canEdit ? (
					<form
						onSubmit={handleCreateReport}
						className="space-y-4 rounded-3xl border border-slate-200 bg-white/80 p-4 shadow-inner"
					>
						<label
							htmlFor="reportRequest"
							className="text-xs font-semibold uppercase tracking-[0.18em] text-slate-500"
						>
							レポートに対するリクエスト（任意）
						</label>
						<textarea
							id="reportRequest"
							value={reportRequest}
							onChange={(event) => setReportRequest(event.target.value)}
							rows={3}
							maxLength={1200}
							className="w-full resize-none rounded-2xl border border-slate-200 bg-white px-3 py-2 text-sm text-slate-800 placeholder:text-slate-400 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-slate-200"
							placeholder="例:「共有している価値観について重点的に分析してほしい」「易しい言葉を使った分かりやすいレポートを出力してほしい」"
						/>
						<Button
							type="submit"
							size="lg"
							disabled={creatingReport}
							isLoading={creatingReport}
							className="w-full justify-center gap-2 rounded-2xl py-6 text-base shadow-lg shadow-slate-900/10"
						>
							<FileText className="h-4 w-4" />
							新しいレポートを生成
						</Button>
					</form>
				) : (
					<div className="rounded-3xl border border-slate-200/70 bg-slate-50/80 px-4 py-3 text-xs text-slate-500">
						レポート生成はセッションのホストのみ利用できます。
					</div>
				)}
			</CardContent>
		</Card>
	);
};
