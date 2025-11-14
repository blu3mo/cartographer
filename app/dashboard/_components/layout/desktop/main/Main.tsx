"use client";

import type { ReactNode } from "react";

type MainProps = {
  header: ReactNode;
  group: ReactNode;
  mobileSessions?: ReactNode;
};

export function Main({ header, mobileSessions, group }: MainProps) {
  return (
    <div className="flex min-h-0 flex-1 flex-col bg-slate-50">
      <div className="flex-shrink-0 border-b border-slate-200 bg-white px-8 py-5">
        {header}
      </div>
      {mobileSessions && (
        <div className="border-b border-slate-200 bg-white px-6 py-4 lg:hidden">
          {mobileSessions}
        </div>
      )}
      <div className="flex min-h-0 flex-1 overflow-hidden px-6 py-6">
        {group}
      </div>
    </div>
  );
}
