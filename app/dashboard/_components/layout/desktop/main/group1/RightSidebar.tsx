"use client";

import type { ReactNode } from "react";

type RightSidebarProps = {
  summary: ReactNode;
  actions?: ReactNode;
};

export function RightSidebar({ summary, actions }: RightSidebarProps) {
  return (
    <div className="flex h-full flex-col gap-4">
      <div className="space-y-2">{summary}</div>
      {actions && <div className="mt-auto flex flex-col gap-2">{actions}</div>}
    </div>
  );
}
