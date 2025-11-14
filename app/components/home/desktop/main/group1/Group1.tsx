"use client";

import type { ReactNode } from "react";

type Group1Props = {
  report: ReactNode;
  chat: ReactNode;
  rightSidebar: ReactNode;
};

export function Group1({ report, chat, rightSidebar }: Group1Props) {
  return (
    <div className="flex min-h-0 flex-1 gap-6">
      <div className="flex min-h-0 flex-1 flex-col gap-6">
        <div className="flex min-h-0 flex-1 flex-col overflow-hidden rounded-3xl border border-slate-200 bg-white shadow-sm">
          {report}
        </div>
        <div className="flex flex-shrink-0 items-center justify-between rounded-3xl border border-slate-200 bg-white px-6 py-5 shadow-sm">
          {chat}
        </div>
      </div>
      <aside className="hidden w-[385px] flex-shrink-0 flex-col gap-4 rounded-3xl border border-slate-200 bg-white px-5 py-5 shadow-sm xl:flex">
        {rightSidebar}
      </aside>
    </div>
  );
}
