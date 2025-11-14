"use client";

import { Info, Link2, Users } from "lucide-react";

import type { AdminSidebarSession } from "@/admin/_components/AdminSidebar";
import {
  Sidebar,
  SidebarContent,
  SidebarFooter,
  SidebarHeader,
  SidebarMenu,
  SidebarMenuButton,
  SidebarMenuItem,
  SidebarSeparator,
} from "@/admin/_components/ui/sidebar";
import { cn } from "@/lib/utils";

type SidebarRightProps = React.ComponentProps<typeof Sidebar> & {
  session?: AdminSidebarSession | null;
  summaryStats?: Array<{ label: string; value: string; hint: string }>;
  fallback?: React.ReactNode;
  showHeader?: boolean;
  showFooter?: boolean;
  showSummary?: boolean;
  children?: React.ReactNode;
};

export function SidebarRight({
  session,
  summaryStats = [],
  fallback,
  children,
  className,
  showHeader = true,
  showFooter = true,
  showSummary = true,
  ...props
}: SidebarRightProps) {
  const isUnavailable = !session;
  const content = !isUnavailable ? children : fallback;

  return (
    <Sidebar
      side="right"
      collapsible="none"
      className={cn(
        "hidden h-svh w-[16rem] min-w-[16rem] border-l bg-white lg:flex",
        className,
      )}
      {...props}
    >
      {showHeader && (
        <SidebarHeader className="border-b border-sidebar-border px-4 py-5">
          {session ? (
            <div className="space-y-1">
              <p className="text-xs font-semibold uppercase tracking-[0.2em] text-slate-400">
                現在のセッション
              </p>
              <div className="flex items-center gap-2">
                <h2 className="text-lg font-semibold text-slate-900 line-clamp-1">
                  {session.title || "名称未設定"}
                </h2>
                <span
                  className={cn(
                    "rounded-full px-2.5 py-0.5 text-[11px] font-semibold",
                    session.isPublic
                      ? "border border-emerald-200 bg-emerald-50 text-emerald-700"
                      : "border border-slate-200 bg-slate-50 text-slate-600",
                  )}
                >
                  {session.isPublic ? "公開" : "非公開"}
                </span>
              </div>
              <p className="text-xs text-slate-500 line-clamp-2">
                {session.goal?.trim() ||
                  session.context?.trim() ||
                  "セッションの目的はまだ設定されていません。"}
              </p>
            </div>
          ) : (
            <div className="flex items-center gap-3 text-sm text-slate-500">
              <Info className="h-4 w-4" />
              セッションを選択してください
            </div>
          )}
        </SidebarHeader>
      )}
      <SidebarContent className="flex flex-col gap-3 overflow-hidden px-4 py-4">
        {showHeader && showSummary && summaryStats.length > 0 && (
          <div className="grid gap-2">
            {summaryStats.map((stat) => (
              <div
                key={stat.label}
                className="rounded-2xl border border-slate-100 bg-slate-50 px-3 py-2"
              >
                <p className="text-[11px] font-semibold uppercase tracking-[0.2em] text-slate-400">
                  {stat.label}
                </p>
                <p className="text-xl font-bold text-slate-900">{stat.value}</p>
                <p className="text-[10px] text-slate-500">{stat.hint}</p>
              </div>
            ))}
          </div>
        )}
        {showHeader && showSummary && <SidebarSeparator className="mx-0" />}
        <div className="flex-1 overflow-y-auto">
          {content || (
            <div className="flex h-full items-center justify-center text-sm text-slate-500">
              サイドパネルを表示するにはセッションを選択してください。
            </div>
          )}
        </div>
      </SidebarContent>
      {showFooter && (
        <SidebarFooter className="border-t border-slate-100 px-4 py-4">
          {session ? (
            <SidebarMenu>
              <SidebarMenuItem>
                <SidebarMenuButton asChild>
                  <a
                    href={`/sessions/${session.id}`}
                    target="_blank"
                    rel="noreferrer"
                  >
                    <Link2 className="h-4 w-4" />
                    <span>参加用ページを開く</span>
                  </a>
                </SidebarMenuButton>
              </SidebarMenuItem>
            </SidebarMenu>
          ) : (
            <SidebarMenu>
              <SidebarMenuItem>
                <SidebarMenuButton disabled>
                  <Users className="h-4 w-4" />
                  <span>セッション未選択</span>
                </SidebarMenuButton>
              </SidebarMenuItem>
            </SidebarMenu>
          )}
        </SidebarFooter>
      )}
    </Sidebar>
  );
}
