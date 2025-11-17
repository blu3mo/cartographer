"use client";

import type { FC } from "react";

import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";

import type { ParticipantProgress } from "./page";

type AnswerProgressProps = {
	participants: ParticipantProgress[];
	rankedParticipants: ParticipantProgress[];
	ParticipantProgressRow: FC<{ participant: ParticipantProgress }>;
};

export const AnswerProgress: FC<AnswerProgressProps> = ({
	participants,
	rankedParticipants,
	ParticipantProgressRow,
}) => {
	return (
		<Card className="border-none bg-white/80 shadow-sm">
			<CardHeader className="pb-4">
				<CardTitle className="text-lg">回答状況</CardTitle>
				<CardDescription>
					誰がどこまで回答したかを確認できます
				</CardDescription>
			</CardHeader>
			<CardContent className="space-y-6">
				{participants.length === 0 ? (
					<p className="text-sm text-slate-500">
						まだ参加者はいません。リンクを共有して参加を促しましょう。
					</p>
				) : (
					<div className="grid gap-2.5 sm:grid-cols-2 xl:grid-cols-3">
						{rankedParticipants.map((participant) => (
							<ParticipantProgressRow
								key={participant.userId}
								participant={participant}
							/>
						))}
					</div>
				)}
			</CardContent>
		</Card>
	);
};

