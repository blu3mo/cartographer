"use client";

import type { FC } from "react";

import { Button } from "@/components/ui/Button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";

import type { EventThreadResponse } from "./page";
import { ThreadEventBubble } from "./page";

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
