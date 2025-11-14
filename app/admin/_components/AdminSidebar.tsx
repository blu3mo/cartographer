"use client";

import { Loader2 } from "lucide-react";
import { useCallback, useMemo, useState, type ReactNode } from "react";

import { Avatar, AvatarFallback } from "@/admin/_components/ui/avatar";
import {
  Sidebar,
  SidebarContent,
  SidebarFooter,
  SidebarGroup,
  SidebarGroupContent,
  SidebarHeader,
  SidebarInput,
  SidebarMenu,
  SidebarMenuButton,
  SidebarMenuItem,
  useSidebar,
} from "@/admin/_components/ui/sidebar";
import { cn } from "@/lib/utils";

export type AdminSidebarSession = {
  id: string;
  title: string;
  context: string;
  goal: string;
  hostUserId: string;
  adminAccessToken?: string;
  createdAt: string;
  isPublic: boolean;
  _count: {
    participants: number;
    statements: number;
  };
  isHost: boolean;
  isParticipant: boolean;
};

type AdminSidebarProps = React.ComponentProps<typeof Sidebar> & {
  sessions: AdminSidebarSession[];
  loading: boolean;
  currentUserId?: string | null;
  selectedSessionId?: string | null;
  onSelectSession: (sessionId: string) => void;
};

const categories = [
  {
    key: "manageable",
    label: "管理中",
    description: "あなたが管理できるセッション",
  },
  {
    key: "participant",
    label: "参加中",
    description: "参加者として招待されています",
  },
  {
    key: "discover",
    label: "公開",
    description: "閲覧可能な公開セッション",
  },
] as const;

type CategoryKey = (typeof categories)[number]["key"];

export function AdminSidebar({
  sessions,
  loading,
  currentUserId,
  selectedSessionId,
  onSelectSession,
  ...props
}: AdminSidebarProps) {
  const { setOpen } = useSidebar();
  const [activeCategory, setActiveCategory] =
    useState<CategoryKey>("manageable");
  const [searchTerm, setSearchTerm] = useState("");
  const [onlyPublic, setOnlyPublic] = useState(false);

  const groupedSessions = useMemo(
    () => ({
      manageable: sessions.filter((session) => session.isHost),
      participant: sessions.filter(
        (session) => !session.isHost && session.isParticipant,
      ),
      discover: sessions.filter(
        (session) => !session.isHost && !session.isParticipant,
      ),
    }),
    [sessions],
  );

  const stats = useMemo(
    () =>
      categories.reduce(
        (acc, category) => ({
          ...acc,
          [category.key]: groupedSessions[category.key].length,
        }),
        {} as Record<CategoryKey, number>,
      ),
    [groupedSessions],
  );

  const visibleSessions = useMemo(() => {
    const base = groupedSessions[activeCategory] ?? [];
    const filteredByPublic = onlyPublic
      ? base.filter((session) => session.isPublic)
      : base;
    if (!searchTerm) return filteredByPublic;
    const normalized = searchTerm.trim().toLowerCase();
    return filteredByPublic.filter((session) => {
      const title = session.title?.toLowerCase() ?? "";
      const goal = session.goal?.toLowerCase() ?? "";
      const context = session.context?.toLowerCase() ?? "";
      return (
        title.includes(normalized) ||
        goal.includes(normalized) ||
        context.includes(normalized)
      );
    });
  }, [groupedSessions, activeCategory, onlyPublic, searchTerm]);

  const handleSessionSelect = useCallback(
    (sessionId: string) => {
      onSelectSession(sessionId);
      setOpen(true);
    },
    [onSelectSession, setOpen],
  );

  return (
    <Sidebar {...props}>
      <SidebarHeader className="space-y-4 border-b border-sidebar-border/60">
        <div className="flex items-center gap-3 rounded-2xl border border-sidebar-border bg-white/80 px-3 py-2">
          <Avatar className="h-10 w-10">
            <AvatarFallback>
              {currentUserId ? currentUserId.slice(0, 2).toUpperCase() : "??"}
            </AvatarFallback>
          </Avatar>
          <div className="min-w-0">
            <p className="truncate text-sm font-semibold text-sidebar-foreground">
              {currentUserId ?? "ゲストユーザー"}
            </p>
            <p className="truncate text-xs text-sidebar-foreground/70">
              Cartographer Workspace
            </p>
          </div>
        </div>
        <SidebarInput
          placeholder="タイトルや目的で検索"
          value={searchTerm}
          onChange={(event) => setSearchTerm(event.target.value)}
        />

        <SidebarMenu className="grid grid-cols-3 gap-2">
        {categories.map((category) => (
            <SidebarMenuItem key={category.key}>
              <SidebarMenuButton
                onClick={() => setActiveCategory(category.key)}
                isActive={activeCategory === category.key}
                className="h-auto flex-col items-start gap-1 rounded-2xl border border-transparent px-3 py-3 text-left"
              >
                <span className="text-[11px] font-semibold uppercase tracking-[0.18em] text-sidebar-foreground/70">
                  {category.label}
                </span>
                <span className="text-2xl font-bold text-sidebar-foreground">
                  {stats[category.key] ?? 0}
                </span>
                <span className="text-[10px] text-sidebar-foreground/70">
                  {category.description}
                </span>
              </SidebarMenuButton>
            </SidebarMenuItem>
          ))}
        </SidebarMenu>
        <FilterToggle
          checked={onlyPublic}
          onCheckedChange={setOnlyPublic}
          className="justify-end"
        />
      </SidebarHeader>


      <SidebarContent className="p-0">
        <SidebarGroup className="h-full rounded-none bg-transparent p-0">
          <SidebarGroupContent className="divide-y">
            {loading ? (
              <SidebarPlaceholder>
                <Loader2 className="h-4 w-4 animate-spin" />
                <span>セッションを読み込んでいます…</span>
              </SidebarPlaceholder>
            ) : visibleSessions.length ? (
              visibleSessions.map((session) => {
                const isActive = selectedSessionId === session.id;
                return (
                  <button
                    key={session.id}
                    type="button"
                    onClick={() => handleSessionSelect(session.id)}
                    className={cn(
                      "flex w-full flex-col gap-2 px-4 py-3 text-left transition hover:bg-sidebar-accent/40",
                      isActive && "bg-sidebar-accent/60",
                    )}
                  >
                    <div className="flex items-center gap-2">
                      <p className="truncate text-sm font-semibold text-sidebar-foreground">
                        {session.title || "タイトル未設定"}
                      </p>
                      {/* <span className="ml-auto text-[11px] text-sidebar-foreground/70">
                        {new Date(session.createdAt).toLocaleDateString("ja-JP")}
                      </span> */}
                      {!session.isPublic && (
                        <span className="rounded-full bg-amber-100 px-2 py-0.5 text-amber-700">
                          非公開
                        </span>
                      )}
                    </div>
                    <p className="line-clamp-2 text-xs text-sidebar-foreground/80">
                      {session.goal ||
                        session.context ||
                        "説明がまだ入力されていません。"}
                    </p>
                    <div className="flex flex-wrap items-center gap-3 text-[11px] text-sidebar-foreground/70">
                      <span>参加者: {session._count.participants}</span>
                      <span>質問数: {session._count.statements}</span>
                      <span className="ml-auto text-[11px] text-sidebar-foreground/70">
                        作成日: {new Date(session.createdAt).toLocaleDateString("ja-JP")}
                      </span>
                      {/* {!session.isPublic && (
                        <span className="rounded-full bg-amber-100 px-2 py-0.5 text-amber-700">
                          非公開
                        </span>
                      )} */}
                    </div>
                  </button>
                );
              })
            ) : (
              <SidebarPlaceholder>
                <span>表示できるセッションがありません。</span>
                <span className="text-xs">
                  フィルターや検索条件を変更して再度お試しください。
                </span>
              </SidebarPlaceholder>
            )}
          </SidebarGroupContent>
        </SidebarGroup>
      </SidebarContent>
      <SidebarFooter className="border-t border-sidebar-border/60">
        <p className="text-xs text-sidebar-foreground/70">
          合計 {sessions.length} 件のセッションを管理しています。
        </p>
      </SidebarFooter>
    </Sidebar>
  );
}

type FilterToggleProps = {
  checked: boolean;
  onCheckedChange: (value: boolean) => void;
  className?: string;
};

function FilterToggle({
  checked,
  onCheckedChange,
  className,
}: FilterToggleProps) {
  return (
    <div
      className={cn(
        "flex items-center gap-3 text-sm text-sidebar-foreground/80",
        className,
      )}
    >
      <span>公開のみ</span>
      <button
        type="button"
        role="switch"
        aria-checked={checked}
        onClick={() => onCheckedChange(!checked)}
        className={cn(
          "relative flex h-5 w-10 items-center rounded-full border border-sidebar-border bg-white transition",
          checked && "bg-sidebar-primary/90",
        )}
      >
        <span
          className={cn(
            "inline-flex h-4 w-4 translate-x-1 items-center justify-center rounded-full bg-sidebar-foreground text-[10px] font-semibold text-white transition",
            checked && "translate-x-5 bg-white text-sidebar-primary",
          )}
        />
      </button>
    </div>
  );
}

function SidebarPlaceholder({ children }: { children: ReactNode }) {
  return (
    <div className="flex flex-col items-center gap-2 px-4 py-12 text-center text-sm text-sidebar-foreground/70">
      {children}
    </div>
  );
}
