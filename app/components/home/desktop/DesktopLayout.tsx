"use client";

import type { ReactNode } from "react";

type DesktopLayoutProps = {
  header: ReactNode;
  leftSidebar: ReactNode;
  main: ReactNode;
};

export function DesktopLayout({
  header,
  leftSidebar,
  main,
}: DesktopLayoutProps) {
  return (
    <div className="flex min-h-screen flex-col bg-white text-slate-900">
      <div className="flex-shrink-0">{header}</div>
      <div className="flex flex-1 min-h-0 overflow-hidden">
        {leftSidebar}
        <div className="flex min-h-0 flex-1 overflow-hidden">{main}</div>
      </div>
    </div>
  );
}
