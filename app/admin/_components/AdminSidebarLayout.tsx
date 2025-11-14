"use client";

import axios from "axios";
import { Loader2 } from "lucide-react";
import { useCallback, useEffect, useMemo, useState } from "react";

import {
  AdminSidebar,
  type AdminSidebarSession,
} from "@/admin/_components/AdminSidebar";
import { SidebarRight } from "@/admin/_components/sidebar-15/SidebarRight";
import { SidebarInset, SidebarProvider } from "@/admin/_components/ui/sidebar";
import { AppHeader } from "@/components/AppHeader";
import { createAuthorizationHeader } from "@/lib/auth";
import { useUserId } from "@/lib/useUserId";
import {
  SessionAdminDashboard,
  type SessionAdminSidebarPayload,
} from "@/sessions/_components/SessionAdminDashboard";

export function AdminSidebarLayout() {
  const { userId, isLoading: userLoading } = useUserId();
  const [sessions, setSessions] = useState<AdminSidebarSession[]>([]);
  const [sessionsLoading, setSessionsLoading] = useState(true);
  const [sessionsError, setSessionsError] = useState<string | null>(null);
  const [selectedSessionId, setSelectedSessionId] = useState<string | null>(
    null,
  );
  const [rightSidebar, setRightSidebar] =
    useState<SessionAdminSidebarPayload | null>(null);

  useEffect(() => {
    if (!userId || userLoading) return;

    let active = true;
    const fetchSessions = async () => {
      try {
        setSessionsLoading(true);
        const response = await axios.get("/api/sessions", {
          headers: createAuthorizationHeader(userId),
        });
        if (!active) return;
        const manageable = (response.data.sessions ?? []).filter(
          (session: AdminSidebarSession) => session.isHost,
        );
        setSessions(manageable);
        setSessionsError(null);
      } catch (error) {
        if (!active) return;
        console.error("Failed to fetch sessions for admin dashboard", error);
        setSessionsError("セッションの取得に失敗しました。");
      } finally {
        if (active) {
          setSessionsLoading(false);
        }
      }
    };

    fetchSessions();
    return () => {
      active = false;
    };
  }, [userId, userLoading]);

  useEffect(() => {
    if (!sessions.length) {
      setSelectedSessionId(null);
      return;
    }
    setSelectedSessionId((current) => {
      if (current && sessions.some((session) => session.id === current)) {
        return current;
      }
      const manageable = sessions.find((session) => session.isHost);
      return (manageable ?? sessions[0]).id;
    });
  }, [sessions]);

  const selectedSession = useMemo(
    () => sessions.find((session) => session.id === selectedSessionId) ?? null,
    [sessions, selectedSessionId],
  );

  const handleSessionSelect = useCallback((sessionId: string) => {
    setSelectedSessionId(sessionId);
  }, []);

  const isLoadingState = sessionsLoading || userLoading;

  useEffect(() => {
    setRightSidebar(null);
  }, [selectedSessionId]);

  const mainContent = (() => {
    const renderStatus = (status: Parameters<typeof StatusMessage>[0]) => (
      <div className="flex h-full flex-1 items-center justify-center">
        <StatusMessage {...status} />
      </div>
    );

    if (isLoadingState) {
      return renderStatus({
        icon: <Loader2 className="h-5 w-5 animate-spin text-slate-400" />,
        title: "読み込み中です",
        description: "セッション情報を取得しています。",
      });
    }

    if (sessionsError) {
      return renderStatus({
        title: "データを取得できませんでした",
        description: sessionsError,
      });
    }

    if (!sessions.length) {
      return renderStatus({
        title: "表示できるセッションがありません",
        description:
          "新しくセッションを作成するか、アクセス権限をご確認ください。",
      });
    }

    if (!selectedSession) {
      return renderStatus({
        title: "セッションを選択してください",
        description: "左側のリストから表示したいセッションを選択します。",
      });
    }

    if (!selectedSession.adminAccessToken) {
      return renderStatus({
        title: "詳細を表示できません",
        description: "このセッションの管理権限がありません。",
      });
    }

    return (
      <SessionAdminDashboard
        key={selectedSession.id}
        sessionId={selectedSession.id}
        accessToken={selectedSession.adminAccessToken}
        embedded
        disableLocalSidebar
        onRightSidebarRender={setRightSidebar}
      />
    );
  })();

  return (
    <SidebarProvider>
      <div className="flex min-h-screen flex-col bg-background">
        <AppHeader />
        <div className="flex flex-1 min-h-0 overflow-hidden bg-slate-50">
          <AdminSidebar
            sessions={sessions}
            loading={isLoadingState}
            currentUserId={userId}
            selectedSessionId={selectedSessionId}
            onSelectSession={handleSessionSelect}
          />
          <SidebarInset className="flex flex-1 flex-col overflow-hidden bg-white">
            <div className="flex-1 min-h-0 overflow-y-auto bg-slate-50 px-2 py-2 sm:px-4 sm:py-4">
              {mainContent}
            </div>
          </SidebarInset>
          <SidebarRight
            session={rightSidebar?.session ?? selectedSession}
            summaryStats={rightSidebar?.summaryStats}
            fallback={
              <div className="flex h-full items-center justify-center px-4 text-xs text-slate-500">
                セッションを選択すると詳細が表示されます
              </div>
            }
          >
            {rightSidebar?.render ? rightSidebar.render() : null}
          </SidebarRight>
        </div>
      </div>
    </SidebarProvider>
  );
}

function StatusMessage({
  title,
  description,
  icon,
}: {
  title: string;
  description: string;
  icon?: React.ReactNode;
}) {
  return (
    <div className="flex h-full flex-col items-center justify-center gap-3 px-4 text-center text-slate-600">
      {icon}
      <div>
        <p className="text-base font-semibold">{title}</p>
        <p className="text-sm text-slate-500">{description}</p>
      </div>
    </div>
  );
}
