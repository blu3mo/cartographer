"use client";

import type { FC } from "react";

import { Button } from "@/components/ui/Button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Input } from "@/components/ui/input";

import type { SessionAdminData } from "./page";

type SessionInfoProps = {
	data: SessionAdminData;
	canEdit: boolean;
	isEditingSettings: boolean;
	editingTitle: string;
	editingContext: string;
	editingGoal: string;
	editingVisibility: "public" | "private";
	isSavingSettings: boolean;
	settingsMessage: string | null;
	settingsError: string | null;
	setIsEditingSettings: (value: boolean) => void;
	setSettingsMessage: (value: string | null) => void;
	setSettingsError: (value: string | null) => void;
	setEditingTitle: (value: string) => void;
	setEditingContext: (value: string) => void;
	setEditingGoal: (value: string) => void;
	setEditingVisibility: (value: "public" | "private") => void;
	handleSaveSettings: (event: React.FormEvent) => void;
	truncateText: (text: string, maxLength: number) => string;
};

export const SessionInfo: FC<SessionInfoProps> = ({
	data,
	canEdit,
	isEditingSettings,
	editingTitle,
	editingContext,
	editingGoal,
	editingVisibility,
	isSavingSettings,
	settingsMessage,
	settingsError,
	setIsEditingSettings,
	setSettingsMessage,
	setSettingsError,
	setEditingTitle,
	setEditingContext,
	setEditingGoal,
	setEditingVisibility,
	handleSaveSettings,
	truncateText,
}) => {
	return (
		<Card className="border-none bg-white/80 shadow-sm">
			<CardHeader className="pb-4">
				<div className="flex items-start justify-between gap-4">
					<div>
						<CardTitle className="text-lg">セッション情報</CardTitle>
						<CardDescription>
							{canEdit
								? "基本情報を編集してアップデートできます"
								: "セッションの基本情報"}
						</CardDescription>
					</div>
					{!isEditingSettings && canEdit && (
						<Button
							variant="ghost"
							size="sm"
							onClick={() => setIsEditingSettings(true)}
							className="gap-1.5 text-xs"
						>
							編集
						</Button>
					)}
				</div>
			</CardHeader>
			<CardContent className="space-y-5">
				{!isEditingSettings ? (
					<div className="space-y-4 text-sm text-slate-600">
						<div>
							<p className="text-xs font-medium text-slate-500 uppercase tracking-[0.12em]">
								公開設定
							</p>
							<p className="mt-1 text-slate-800 font-medium">
								{data.isPublic ? "公開" : "非公開"}
							</p>
						</div>
						<div>
							<p className="text-xs font-medium text-slate-500 uppercase tracking-[0.12em]">
								ゴール
							</p>
							<p className="mt-1 leading-relaxed" title={data.goal ?? undefined}>
								{data.goal ? truncateText(data.goal, 160) : "未設定"}
							</p>
						</div>
						<div>
							<p className="text-xs font-medium text-slate-500 uppercase tracking-[0.12em]">
								背景情報
							</p>
							<p
								className="mt-1 leading-relaxed whitespace-pre-wrap"
								title={data.context ?? undefined}
							>
								{data.context ? truncateText(data.context, 160) : "未設定"}
							</p>
						</div>
					</div>
				) : (
					<form onSubmit={handleSaveSettings} className="space-y-4">
						<div className="space-y-1.5">
							<label
								htmlFor="sessionTitle"
								className="text-xs font-medium text-slate-600"
							>
								タイトル
							</label>
							<Input
								id="sessionTitle"
								type="text"
								value={editingTitle}
								onChange={(event) => setEditingTitle(event.target.value)}
								required
								className="text-sm"
							/>
						</div>

						<div className="space-y-1.5">
							<span className="text-xs font-medium text-slate-600">公開設定</span>
							<div className="grid grid-cols-2 gap-2">
								<label className="flex flex-1 items-center gap-2 rounded-xl border border-slate-200 bg-slate-50/60 px-3 py-2 text-xs text-slate-700 shadow-sm">
									<input
										type="radio"
										name="sessionVisibility"
										value="public"
										checked={editingVisibility === "public"}
										onChange={() => setEditingVisibility("public")}
									/>
									<span>公開</span>
								</label>
								<label className="flex flex-1 items-center gap-2 rounded-xl border border-slate-200 bg-slate-50/60 px-3 py-2 text-xs text-slate-700 shadow-sm">
									<input
										type="radio"
										name="sessionVisibility"
										value="private"
										checked={editingVisibility === "private"}
										onChange={() => setEditingVisibility("private")}
									/>
									<span>非公開</span>
								</label>
							</div>
						</div>

						<div className="space-y-1.5">
							<label
								htmlFor="sessionGoal"
								className="text-xs font-medium text-slate-600"
							>
								ゴール
							</label>
							<textarea
								id="sessionGoal"
								value={editingGoal}
								onChange={(event) => setEditingGoal(event.target.value)}
								required
								rows={5}
								className="flex w-full rounded-xl border border-slate-200 bg-white px-3 py-2 text-sm text-slate-800 shadow-sm placeholder:text-slate-400 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-slate-200 resize-none"
							/>
						</div>

						<div className="space-y-1.5">
							<label
								htmlFor="sessionContext"
								className="text-xs font-medium text-slate-600"
							>
								背景情報
							</label>
							<textarea
								id="sessionContext"
								value={editingContext}
								onChange={(event) => setEditingContext(event.target.value)}
								rows={5}
								className="flex w-full rounded-xl border border-slate-200 bg-white px-3 py-2 text-sm text-slate-800 shadow-sm placeholder:text-slate-400 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-slate-200 resize-none"
							/>
						</div>

						{(settingsMessage || settingsError) && (
							<div
								className={`rounded-xl px-3 py-2 text-xs ${settingsError ? "bg-red-50 text-red-600" : "bg-emerald-50 text-emerald-700"}`}
							>
								{settingsError ?? settingsMessage}
							</div>
						)}

						<div className="flex items-center gap-2">
							<Button
								type="submit"
								disabled={isSavingSettings}
								isLoading={isSavingSettings}
								size="sm"
								className="gap-1.5 text-xs"
							>
								保存
							</Button>
							<Button
								type="button"
								variant="ghost"
								size="sm"
								onClick={() => {
									setIsEditingSettings(false);
									setSettingsMessage(null);
									setSettingsError(null);
									setEditingTitle(data.title);
									setEditingContext(data.context);
									setEditingGoal(data.goal);
									setEditingVisibility(data.isPublic ? "public" : "private");
								}}
								className="gap-1.5 text-xs"
							>
								キャンセル
							</Button>
						</div>
					</form>
				)}
			</CardContent>
		</Card>
	);
};
