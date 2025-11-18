"use client";

import type { FC, ReactElement } from "react";

import { Bot } from "lucide-react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";

import { Button } from "@/components/ui/Button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";

import type { EventThreadResponse, TimelineEvent } from "./page";

type SessionLogProps = {
	threadData: EventThreadResponse | null;
	threadLoading: boolean;
	threadError: string | null;
	threadContainerRef: React.RefObject<HTMLDivElement | null>;
	expandedEvents: Record<string, boolean>;
	setExpandedEvents: (updater: (prev: Record<string, boolean>) => Record<string, boolean>) => void;
	canEdit: boolean;
	messageDraft: string;
	setMessageDraft: (value: string) => void;
	sendingMessage: boolean;
	handleSendMessage: () => void;
};

export const SessionLog: FC<SessionLogProps> = ({
	threadData,
	threadLoading,
	threadError,
	threadContainerRef,
	expandedEvents,
	setExpandedEvents,
	canEdit,
	messageDraft,
	setMessageDraft,
	sendingMessage,
	handleSendMessage,
}) => {
	return (
		<Card className="border-none bg-white/80 shadow-lg">
			<CardHeader className="pb-4">
				<div className="flex flex-wrap items-start justify-between gap-4">
					<div>
						<CardTitle className="text-lg">進行ログ</CardTitle>
						<CardDescription>
							ファシリテーターAIの進行状況をここから確認できます
						</CardDescription>
					</div>
				</div>
			</CardHeader>
			<CardContent className="space-y-6">
				<div className="relative overflow-hidden rounded-3xl border border-slate-200 bg-white/70 shadow-inner">
					{threadLoading && (
						<div className="absolute inset-x-0 top-0 z-10 bg-gradient-to-b from-white/90 to-white/30 py-2 text-center text-xs text-slate-500">
							更新中…
						</div>
					)}
					<div
						ref={threadContainerRef}
						className="h-[620px] overflow-y-auto px-6 py-6 space-y-5"
					>
						{threadError ? (
							<div className="rounded-2xl border border-amber-200 bg-amber-50 px-4 py-3 text-sm text-amber-700">
								{threadError}
							</div>
						) : threadData?.events.length ? (
							threadData.events.map((event) => {
								const isHostMessage = event.type === "user_message";
								const expanded = Boolean(expandedEvents[event.id]);
								return (
									<ThreadEventBubble
										key={event.id}
										event={event}
										isHostMessage={isHostMessage}
										expanded={expanded}
										onToggle={() =>
											setExpandedEvents((prev) => ({
												...prev,
												[event.id]: !prev[event.id],
											}))
										}
									/>
								);
							})
						) : (
							<p className="text-sm text-slate-500">
								まだイベントはありません。Agentとの会話はここに表示されます。
							</p>
						)}
					</div>
				</div>

				{canEdit && (
					<div className="space-y-2 rounded-3xl border border-slate-200 bg-white/80 p-4 shadow-sm">
						<label
							htmlFor="adminMessage"
							className="text-xs font-medium text-slate-600"
						>
							ファシリテーターAIへのメッセージ
						</label>
						<textarea
							id="adminMessage"
							value={messageDraft}
							onChange={(event) => setMessageDraft(event.target.value)}
							rows={3}
							className="w-full rounded-2xl border border-slate-200 bg-white px-3 py-2 text-sm text-slate-800 placeholder:text-slate-400 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-slate-200 resize-none"
							placeholder="ファシリテーターAIへ伝えたい情報や、与えたい指示を書き込めます。"
						/>
						<div className="flex items-center justify-between">
							<Button
								type="button"
								onClick={handleSendMessage}
								disabled={
									sendingMessage || messageDraft.trim().length === 0
								}
								isLoading={sendingMessage}
								size="sm"
								className="gap-1.5 text-xs"
							>
								送信
							</Button>
						</div>
					</div>
				)}
			</CardContent>
		</Card>
	);
};

const EVENT_TYPE_META: Record<
	TimelineEvent["type"],
	{ label: string; accent: string; badge: string }
> = {
	plan: {
		label: "Plan",
		accent: "text-sky-600",
		badge: "bg-sky-50 text-sky-700 border-sky-200",
	},
	survey: {
		label: "Survey",
		accent: "text-emerald-600",
		badge: "bg-emerald-50 text-emerald-700 border-emerald-200",
	},
	survey_analysis: {
		label: "Analysis",
		accent: "text-purple-600",
		badge: "bg-purple-50 text-purple-700 border-purple-200",
	},
	user_message: {
		label: "You",
		accent: "text-indigo-600",
		badge: "bg-indigo-50 text-indigo-700 border-indigo-200",
	},
};

const formatDateTime = (value: string) => {
	const date = new Date(value);
	const now = new Date();
	const diff = now.getTime() - date.getTime();
	const minutes = Math.floor(diff / 60000);
	const hours = Math.floor(diff / 3600000);
	const days = Math.floor(diff / 86400000);

	if (minutes < 1) return "たった今";
	if (minutes < 60) return `${minutes}分前`;
	if (hours < 24) return `${hours}時間前`;
	if (days < 7) return `${days}日前`;

	return date.toLocaleString("ja-JP", {
		month: "short",
		day: "numeric",
		hour: "2-digit",
		minute: "2-digit",
		hour12: false,
	});
};

type ThreadEventBubbleProps = {
	event: TimelineEvent;
	isHostMessage: boolean;
	expanded: boolean;
	onToggle: () => void;
};

const ThreadEventBubble: FC<ThreadEventBubbleProps> = ({
	event,
	isHostMessage,
	expanded,
	onToggle,
}) => {
	const eventMeta = EVENT_TYPE_META[event.type];
	const timestamp = formatDateTime(event.createdAt);
	const payload = event.payload ?? {};

	const message =
		typeof payload.message === "string"
			? payload.message
			: typeof payload.content === "string"
				? payload.content
				: "";

	const showAgentHeader = !!event.agentId && !isHostMessage;

	return (
		<div className="flex gap-3">
			<div className="mt-1 flex h-7 w-7 items-center justify-center rounded-full bg-slate-100 text-slate-500">
				{isHostMessage ? (
					<span className="text-[10px] font-semibold">YOU</span>
				) : (
					<Bot className="h-4 w-4" />
				)}
			</div>
			<div className="flex-1 space-y-1">
				<div className="flex items-center gap-2">
					<span
						className={`inline-flex items-center gap-1 rounded-full border px-2 py-0.5 text-[10px] font-medium ${eventMeta.badge}`}
					>
						<span className="h-1.5 w-1.5 rounded-full bg-current" />
						{eventMeta.label}
					</span>
					<span className="text-[11px] text-slate-400">{timestamp}</span>
				</div>

				<div className="relative inline-block max-w-full rounded-2xl bg-slate-50 px-3 py-2 text-sm text-slate-800 shadow-sm">
					{showAgentHeader && (
						<div className="mb-1 flex items-center gap-1 text-[11px] font-medium text-slate-500">
							<Bot className="h-3 w-3" />
							<span>ファシリテーターAI</span>
						</div>
					)}
					{message ? (
						<div className="prose prose-sm max-w-none text-slate-800">
							<ReactMarkdown remarkPlugins={[remarkGfm]}>{
								message
							}</ReactMarkdown>
						</div>
					) : (
						<p className="text-xs text-slate-400">内容がありません。</p>
					)}
					{event.statements?.length ? (
						<button
							type="button"
							onClick={onToggle}
							className="mt-2 inline-flex items-center gap-1 rounded-full bg-white/70 px-2 py-0.5 text-[11px] text-slate-500 shadow-sm ring-1 ring-slate-200 hover:bg-white"
						>
							<span>
								{expanded
									? "関連するステートメントを隠す"
									: "関連するステートメントを表示"}
							</span>
						</button>
					) : null}
				</div>

				{expanded && event.statements?.length ? (
					<div className="mt-2 space-y-1 rounded-2xl bg-slate-50/80 p-2 text-xs text-slate-600">
						{event.statements.map((statement) => (
							<div
								key={statement.id}
								className="rounded-xl bg-white/80 px-2 py-1 shadow-sm ring-1 ring-slate-100"
							>
								<span className="mr-1 inline-flex h-4 w-4 items-center justify-center rounded-full bg-slate-100 text-[10px] font-semibold text-slate-500">
									{statement.orderIndex + 1}
								</span>
								<span>{statement.text}</span>
							</div>
						))}
					</div>
				) : null}
			</div>
		</div>
	);
};

