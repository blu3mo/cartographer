"use client";

import type { LucideIcon } from "lucide-react";

import {
  CalendarDays,
  ChevronDown,
  ChevronUp,
  ExternalLink,
  FileText,
  Plus,
  Users,
} from "lucide-react";
import { useEffect, useState } from "react";

import { Button } from "@/components/ui/Button";
import { Input } from "@/components/ui/input";
import { LeftSidebar } from "@/dashboard/_components/layout/desktop/leftsidebar/LeftSidebar";
import { LeftSidebarHeader } from "@/dashboard/_components/layout/desktop/leftsidebar/LeftSidebarHeader";
import { Session as LeftSidebarSessionPanel } from "@/dashboard/_components/layout/desktop/leftsidebar/Session";
import { cn } from "@/lib/utils";

import type { Session } from "./types";

export type SessionNavigatorProps = {
  totalSessions: number;
  searchTerm: string;
  onSearchTermChange: (value: string) => void;
  manageableCount: number;
  participantCount: number;
  discoverCount: number;
  adminSessions: Session[];
  participantSessions: Session[];
  discoverSessions: Session[];
  selectedSessionId: string | null;
  onSelectSession: (sessionId: string) => void;
  onNavigateSession: (sessionId: string) => void;
  onCreateSession: () => void;
};

export function SessionNavigatorSidebar({
  totalSessions,
  searchTerm,
  onSearchTermChange,
  manageableCount,
  participantCount,
  discoverCount,
  adminSessions,
  participantSessions,
  discoverSessions,
  selectedSessionId,
  onSelectSession,
  onNavigateSession,
  onCreateSession,
}: SessionNavigatorProps) {
  return (
    <LeftSidebar
      header={
        <LeftSidebarHeader
          title={<span>セッション一覧</span>}
          description={
            <span className="text-slate-400">
              合計 {totalSessions} 件のセッション
            </span>
          }
          toolbar={
            <>
              <Input
                type="search"
                placeholder="タイトルや目的で検索"
                value={searchTerm}
                onChange={(event) => onSearchTermChange(event.target.value)}
                className="flex-1 rounded-xl border border-slate-700 bg-slate-900/70 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500 focus-visible:ring-slate-500"
              />
              <Button
                type="button"
                size="icon"
                variant="secondary"
                className="bg-slate-100 text-slate-900 hover:bg-slate-200"
                onClick={onCreateSession}
              >
                <Plus className="h-4 w-4" />
              </Button>
            </>
          }
        />
      }
      session={
        <LeftSidebarSessionPanel>
          <SessionSummaryCards
            manageableCount={manageableCount}
            participantCount={participantCount}
            discoverCount={discoverCount}
          />
          <SidebarSessionsSection
            title="管理中"
            description="あなたが管理できるセッションです。"
            sessions={adminSessions}
            selectedSessionId={selectedSessionId}
            onSelectSession={onSelectSession}
            mode="select"
          />
          <SidebarSessionsSection
            title="参加中"
            description="参加者として登録されているセッションです。"
            sessions={participantSessions}
            onNavigate={onNavigateSession}
            mode="link"
            collapsible
            defaultCollapsed
          />
          <SidebarSessionsSection
            title="公開セッション"
            description="まだ参加していない公開セッションです。"
            sessions={discoverSessions}
            onNavigate={onNavigateSession}
            mode="link"
            collapsible
            defaultCollapsed
          />
        </LeftSidebarSessionPanel>
      }
    />
  );
}

export function SessionNavigatorMobile(props: SessionNavigatorProps) {
  const {
    manageableCount,
    participantCount,
    discoverCount,
    adminSessions,
    selectedSessionId,
    onSelectSession,
    onCreateSession,
  } = props;

  return (
    <div className="space-y-4 lg:hidden">
      <div className="grid grid-cols-3 gap-2 text-center text-xs text-slate-500">
        <div className="rounded-xl border border-slate-200 bg-white px-3 py-2">
          <p className="font-semibold uppercase tracking-[0.18em] text-slate-500">
            管理中
          </p>
          <p className="text-lg font-bold text-slate-700">{manageableCount}</p>
        </div>
        <div className="rounded-xl border border-slate-200 bg-white px-3 py-2">
          <p className="font-semibold uppercase tracking-[0.18em] text-slate-500">
            参加中
          </p>
          <p className="text-lg font-bold text-slate-700">{participantCount}</p>
        </div>
        <div className="rounded-xl border border-slate-200 bg-white px-3 py-2">
          <p className="font-semibold uppercase tracking-[0.18em] text-slate-500">
            公開
          </p>
          <p className="text-lg font-bold text-slate-700">{discoverCount}</p>
        </div>
      </div>
      <SidebarSessionsSection
        title="管理中"
        description="あなたが管理できるセッションです。"
        sessions={adminSessions}
        selectedSessionId={selectedSessionId}
        onSelectSession={onSelectSession}
        mode="select"
      />
      <Button type="button" className="w-full" onClick={onCreateSession}>
        <Plus className="mr-1 h-4 w-4" />
        新しいセッションを作成
      </Button>
    </div>
  );
}

function SessionSummaryCards({
  manageableCount,
  participantCount,
  discoverCount,
}: {
  manageableCount: number;
  participantCount: number;
  discoverCount: number;
}) {
  return (
    <div className="grid gap-3 text-[11px] uppercase tracking-[0.18em] text-slate-400">
      <div className="rounded-2xl border border-slate-800/50 bg-slate-900/60 px-4 py-3">
        <p className="font-semibold text-slate-300">管理中</p>
        <p className="text-2xl font-bold text-white">{manageableCount}</p>
      </div>
      <div className="rounded-2xl border border-slate-800/40 bg-slate-900/50 px-4 py-3">
        <p className="font-semibold text-slate-300">参加中</p>
        <p className="text-2xl font-bold text-white">{participantCount}</p>
      </div>
      <div className="rounded-2xl border border-slate-800/40 bg-slate-900/50 px-4 py-3">
        <p className="font-semibold text-slate-300">公開</p>
        <p className="text-2xl font-bold text-white">{discoverCount}</p>
      </div>
    </div>
  );
}

type SidebarSessionsSectionProps = {
  title: string;
  description: string;
  sessions: Session[];
  selectedSessionId?: string | null;
  onSelectSession?: (sessionId: string) => void;
  onNavigate?: (sessionId: string) => void;
  mode: "select" | "link";
  collapsible?: boolean;
  defaultCollapsed?: boolean;
};

function SidebarSessionsSection({
  title,
  description,
  sessions,
  selectedSessionId,
  onSelectSession,
  onNavigate,
  mode,
  collapsible = false,
  defaultCollapsed = false,
}: SidebarSessionsSectionProps) {
  const [collapsed, setCollapsed] = useState(defaultCollapsed);

  useEffect(() => {
    setCollapsed(defaultCollapsed);
  }, [defaultCollapsed]);

  const toggleCollapse = () => {
    if (!collapsible) return;
    setCollapsed((previous) => !previous);
  };

  return (
    <div className="space-y-2">
      <button
        type="button"
        className="flex w-full items-center justify-between rounded-xl border border-slate-800/50 bg-slate-900/40 px-3 py-2 text-left text-xs font-semibold uppercase tracking-[0.18em] text-slate-300"
        onClick={toggleCollapse}
        aria-expanded={!collapsed}
      >
        <span>{title}</span>
        {collapsible && (
          <span className="text-slate-400">
            {collapsed ? (
              <ChevronDown className="h-4 w-4" />
            ) : (
              <ChevronUp className="h-4 w-4" />
            )}
          </span>
        )}
      </button>
      <p className="text-[11px] text-slate-400">{description}</p>
      {!collapsed && (
        <div className="space-y-2">
          {sessions.length === 0 ? (
            <p className="rounded-xl border border-dashed border-slate-700 px-3 py-3 text-[11px] text-slate-400">
              {description}
            </p>
          ) : (
            sessions.map((session) => {
              const isActive = selectedSessionId === session.id;
              const context =
                session.context?.trim() || "まだ入力されていません";
              const goal = session.goal?.trim() || "まだ入力されていません";

              const handleClick = () => {
                if (mode === "select") {
                  onSelectSession?.(session.id);
                } else {
                  onNavigate?.(session.id);
                }
              };

              return (
                <div
                  key={session.id}
                  className={cn(
                    "rounded-2xl border bg-slate-900/40 p-3 text-left shadow-sm",
                    isActive
                      ? "border-slate-100/80 bg-slate-100/20 text-white"
                      : "border-transparent text-slate-100",
                  )}
                >
                  <button
                    type="button"
                    onClick={handleClick}
                    className="flex w-full flex-col items-start gap-2 text-left"
                  >
                    <div className="flex w-full items-center gap-2">
                      <p className="text-sm font-semibold">
                        {session.title || "名称未設定"}
                      </p>
                      {!session.isPublic && (
                        <span className="rounded-full bg-slate-100/20 px-2 py-0.5 text-[10px] font-semibold text-slate-200">
                          非公開
                        </span>
                      )}
                      <a
                        href={`/sessions/${session.id}`}
                        target="_blank"
                        rel="noreferrer"
                        onClick={(event) => event.stopPropagation()}
                        className="ml-auto inline-flex items-center justify-center rounded-full border border-white/20 p-1.5 text-slate-200 transition-colors hover:border-white/40 hover:text-white"
                      >
                        <ExternalLink className="h-3.5 w-3.5" />
                        <span className="sr-only">
                          セッションを新しいタブで開く
                        </span>
                      </a>
                    </div>
                    <p className="text-[11px] text-slate-200/80">{context}</p>
                    <p className="text-[11px] text-slate-200/80">{goal}</p>
                  </button>
                  <div className="mt-3 flex flex-wrap items-center gap-x-3 gap-y-1 text-slate-200/80">
                    <SidebarStat
                      icon={Users}
                      label="参加者"
                      value={session._count.participants}
                    />
                    <SidebarStat
                      icon={FileText}
                      label="質問"
                      value={session._count.statements}
                    />
                    <SidebarStat
                      icon={CalendarDays}
                      label="作成日"
                      value={new Date(session.createdAt).toLocaleDateString(
                        "ja-JP",
                      )}
                    />
                  </div>
                </div>
              );
            })
          )}
        </div>
      )}
    </div>
  );
}

function SidebarStat({
  icon: Icon,
  label,
  value,
}: {
  icon: LucideIcon;
  label: string;
  value: string | number;
}) {
  return (
    <span className="inline-flex items-center gap-1.5 text-[11px]">
      <Icon className="h-3.5 w-3.5 text-slate-300" aria-hidden />
      <span className="font-semibold text-white">{value}</span>
      <span className="sr-only">{label}</span>
    </span>
  );
}
