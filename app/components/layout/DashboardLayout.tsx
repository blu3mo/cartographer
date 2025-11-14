"use client";

import type { ReactNode } from "react";

import { AboutCartographerButton } from "@/components/AboutCartographerButton";
import { AppFooter } from "@/components/AppFooter";
import { AppHeader } from "@/components/AppHeader";
import {
  SidebarInset,
  SidebarOverlay,
  SidebarProvider,
  SidebarTrigger,
} from "@/components/ui/sidebar";

type DashboardLayoutProps = {
  sidebar: ReactNode;
  headerContent?: ReactNode;
  headerActions?: ReactNode;
  children: ReactNode;
  showFooter?: boolean;
};

export function DashboardLayout({
  sidebar,
  headerContent,
  headerActions = <AboutCartographerButton />,
  children,
  showFooter = true,
}: DashboardLayoutProps) {
  const defaultHeaderContent = (
    <div className="flex items-center gap-2">
      <span className="text-slate-600">すべてのセッション</span>
    </div>
  );
  const resolvedHeaderContent =
    typeof headerContent === "undefined" ? defaultHeaderContent : headerContent;
  const hasHeaderContent =
    resolvedHeaderContent !== false &&
    resolvedHeaderContent !== null &&
    typeof resolvedHeaderContent !== "undefined";
  const hasHeaderActions =
    headerActions !== null &&
    headerActions !== false &&
    typeof headerActions !== "undefined";
  return (
    <SidebarProvider>
      <div className="flex min-h-screen flex-col bg-background">
        <AppHeader
          rightSlot={
            <>
              <SidebarTrigger className="lg:hidden" />
              {headerActions}
            </>
          }
        />
        <div className="flex flex-1 min-h-0 overflow-hidden bg-slate-50">
          {sidebar}
          <SidebarOverlay />
          <SidebarInset className="flex flex-1 flex-col overflow-hidden bg-white">
            {(hasHeaderContent || hasHeaderActions) && (
              <div className="border-b border-slate-200 bg-white/80 px-4 py-3 shadow-sm sm:px-6 lg:px-8">
                <div className="flex items-center gap-4">
                  {hasHeaderContent && (
                    <div className="min-w-0 flex-1 text-sm font-medium text-slate-700">
                      {resolvedHeaderContent}
                    </div>
                  )}
                  {hasHeaderActions && (
                    <div
                      className={`hidden items-center gap-3 lg:flex${
                        hasHeaderContent ? "" : " lg:ml-auto"
                      }`}
                    >
                      {headerActions}
                    </div>
                  )}
                </div>
              </div>
            )}
            <div className="flex flex-1 min-h-0 overflow-hidden">
              <main className="flex-1 overflow-hidden">{children}</main>
            </div>
            {showFooter && <AppFooter />}
          </SidebarInset>
        </div>
      </div>
    </SidebarProvider>
  );
}
