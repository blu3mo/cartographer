"use client";

import type { FC } from "react";

import { Button } from "@/components/ui/Button";
import {
	Card,
	CardContent,
	CardDescription,
	CardHeader,
	CardTitle,
} from "@/components/ui/card";

import type { StatementHighlight } from "./page";

type HighlightTone = "emerald" | "amber" | "slate";

type StatementHighlightsProps = {
	statementHighlights: {
		agreement: StatementHighlight[];
		conflict: StatementHighlight[];
		dontKnow: StatementHighlight[];
	};
	sessionId: string;
};

export const StatementHighlights: FC<StatementHighlightsProps> = ({
	statementHighlights,
	sessionId,
}) => {
	return (
		<Card className="border-none bg-white/80 shadow-sm">
			<CardHeader className="pb-4">
				<CardTitle className="text-lg">ステートメントのハイライト</CardTitle>
				<CardDescription>
					合意・対立・迷いが大きいテーマを把握できます
				</CardDescription>
			</CardHeader>
			<CardContent className="space-y-6">
				<div className="grid gap-6 lg:grid-cols-3">
					<StatementHighlightColumn
						title="合意度トップ3"
						tone="emerald"
						items={statementHighlights.agreement}
					/>
					<StatementHighlightColumn
						title="対立度トップ3"
						tone="amber"
						items={statementHighlights.conflict}
					/>
					<StatementHighlightColumn
						title="わからない度トップ3"
						tone="slate"
						items={statementHighlights.dontKnow}
					/>
				</div>
				<div className="flex justify-end">
					<Button
						variant="ghost"
						size="sm"
						onClick={() =>
							window.open(
								`/sessions/${sessionId}/admin/statements`,
								"_blank",
							)
						}
						className="gap-1.5 text-xs"
					>
						ステートメント一覧へ
					</Button>
				</div>
			</CardContent>
		</Card>
	);
};

type StatementHighlightColumnProps = {
	title: string;
	tone: HighlightTone;
	items: StatementHighlight[];
};

const StatementHighlightColumn: FC<StatementHighlightColumnProps> = ({
	title,
	tone,
	items,
}) => {
	const toneClass =
		tone === "emerald"
			? "bg-emerald-50 border-emerald-100"
			: tone === "amber"
				? "bg-amber-50 border-amber-100"
				: "bg-slate-50 border-slate-100";

	const badgeClass =
		tone === "emerald"
			? "bg-emerald-100 text-emerald-800"
			: tone === "amber"
				? "bg-amber-100 text-amber-800"
				: "bg-slate-200 text-slate-700";

	const formatPercentage = (value: number) => {
		if (Number.isNaN(value)) return "0%";
		const rounded = Math.round(value * 10) / 10;
		if (Math.abs(rounded - Math.round(rounded)) < 0.05) {
			return `${Math.round(rounded)}%`;
		}
		return `${rounded.toFixed(1)}%`;
	};

	return (
		<div className="space-y-3">
			<h3 className="text-sm font-semibold text-slate-900">{title}</h3>
			{items.length === 0 ? (
				<p className="text-xs text-slate-500">
					まだ十分な回答データがありません。
				</p>
			) : (
				items.map((item, index) => (
					<div
						key={item.statement.id}
						className={`rounded-2xl border px-4 py-4 shadow-sm ${toneClass}`}
					>
						<div className="flex items-start justify-between gap-3">
							<span
								className={`inline-flex items-center justify-center rounded-full px-2 py-0.5 text-[10px] font-semibold ${badgeClass}`}
							>
								#{index + 1}
							</span>
							<div className="text-[11px] text-slate-500">
								回答率 {formatPercentage(item.responseRate)}
							</div>
						</div>
						<p className="mt-3 text-sm text-slate-800 leading-relaxed">
							{item.statement.text}
						</p>
						<div className="mt-4 flex items-center gap-3 text-[11px] text-slate-600">
							<span className="font-medium text-emerald-700">
								Yes {formatPercentage(item.positive)}
							</span>
							<span className="font-medium text-amber-700">
								No {formatPercentage(item.negative)}
							</span>
							<span>わからない {formatPercentage(item.neutral)}</span>
						</div>
					</div>
				))
			)}
		</div>
	);
};
