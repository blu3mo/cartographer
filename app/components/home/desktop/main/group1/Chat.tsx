"use client";

import type { ReactNode } from "react";

type ChatProps = {
  title: ReactNode;
  action?: ReactNode;
  description?: ReactNode;
};

export function Chat({ title, action, description }: ChatProps) {
  return (
    <>
      <div className="space-y-2">
        <div className="text-sm font-semibold text-slate-700">{title}</div>
        {description && (
          <div className="text-sm text-slate-500">{description}</div>
        )}
      </div>
      {action && <div className="flex-shrink-0">{action}</div>}
    </>
  );
}
