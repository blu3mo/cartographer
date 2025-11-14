"use client";

import type { ReactNode } from "react";

type LeftSidebarProps = {
  header: ReactNode;
  session: ReactNode;
};

export function LeftSidebar({ header, session }: LeftSidebarProps) {
  return (
    <aside className="hidden w-[306px] flex-shrink-0 flex-col border-r border-slate-200 bg-slate-950/95 text-slate-100 lg:flex">
      <div className="flex-shrink-0 border-b border-slate-800/60 bg-slate-900/90 px-5 py-4">
        {header}
      </div>
      <div className="flex-1 overflow-y-auto px-5 py-6">{session}</div>
    </aside>
  );
}
