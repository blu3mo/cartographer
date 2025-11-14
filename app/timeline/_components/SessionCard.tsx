import Link from "next/link";
import { CalendarDays, Users, HelpCircle, ArrowRight } from "lucide-react";
import { Button } from "@/components/ui/Button";
import type { PublicSession } from "@/lib/server/public-sessions";

type SessionCardProps = {
  session: PublicSession;
};

export function SessionCard({ session }: SessionCardProps) {
  const createdAt = new Date(session.createdAt);
  const formattedDate = createdAt.toLocaleDateString("ja-JP", {
    year: "numeric",
    month: "long",
    day: "numeric",
  });

  return (
    <div className="group rounded-2xl border border-slate-200 bg-white p-6 shadow-sm transition-all hover:border-blue-300 hover:shadow-lg">
      <div className="flex flex-col gap-4">
        {/* Title */}
        <div className="flex items-start justify-between gap-4">
          <h3 className="text-xl font-semibold text-slate-900 transition-colors group-hover:text-blue-600">
            {session.title || "名称未設定"}
          </h3>
        </div>

        {/* Context */}
        {session.context && (
          <div className="space-y-1">
            <p className="text-xs font-semibold uppercase tracking-wider text-slate-500">
              背景
            </p>
            <p className="text-sm text-slate-600 line-clamp-2">
              {session.context}
            </p>
          </div>
        )}

        {/* Goal */}
        {session.goal && (
          <div className="space-y-1">
            <p className="text-xs font-semibold uppercase tracking-wider text-slate-500">
              目的
            </p>
            <p className="text-sm text-slate-600 line-clamp-2">
              {session.goal}
            </p>
          </div>
        )}

        {/* Stats */}
        <div className="flex flex-wrap items-center gap-x-4 gap-y-2 text-sm text-slate-600">
          <div className="inline-flex items-center gap-2">
            <Users className="h-4 w-4" aria-hidden />
            <span>{session._count.participants} 人参加</span>
          </div>
          <div className="inline-flex items-center gap-2">
            <HelpCircle className="h-4 w-4" aria-hidden />
            <span>{session._count.statements} 質問</span>
          </div>
          <div className="inline-flex items-center gap-2">
            <CalendarDays className="h-4 w-4" aria-hidden />
            <span>{formattedDate}</span>
          </div>
        </div>

        {/* CTA */}
        <div className="mt-2 flex items-center justify-end">
          <Link href={`/sessions/${session.id}`}>
            <Button variant="outline" size="sm" className="group/btn">
              詳細を見る
              <ArrowRight className="ml-1 h-4 w-4 transition-transform group-hover/btn:translate-x-1" />
            </Button>
          </Link>
        </div>
      </div>
    </div>
  );
}
