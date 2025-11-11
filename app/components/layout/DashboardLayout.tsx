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
        >
          <div className="hidden items-center gap-6 text-sm text-muted-foreground lg:flex">
            <SidebarTrigger className="mr-1 shrink-0 rounded-full border border-slate-200 bg-white p-2 text-slate-600" />
            {resolvedHeaderContent}
          </div>
        </AppHeader>
        <div className="flex flex-1 min-h-0 overflow-hidden bg-slate-50">
          {sidebar}
          <SidebarOverlay />
          <SidebarInset className="flex flex-1 flex-col overflow-hidden bg-white">
            <main className="flex-1 overflow-y-auto">{children}</main>
            {showFooter && <AppFooter />}
          </SidebarInset>
        </div>
      </div>
    </SidebarProvider>
  );
}
