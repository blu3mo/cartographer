"use client";

import type { ReactNode } from "react";

type ReportProps = {
  header: ReactNode;
  content: ReactNode;
};

export function Report({ header, content }: ReportProps) {
  return (
    <>
      <div className="border-b border-slate-100 bg-slate-50/80 px-6 py-4">
        {header}
      </div>
      <div className="flex-1 min-h-0 overflow-hidden">{content}</div>
    </>
  );
}
