"use client";

import type { ReactNode } from "react";

type MainHeaderProps = {
  title: ReactNode;
  actions?: ReactNode;
  description?: ReactNode;
};

export function MainHeader({ title, actions, description }: MainHeaderProps) {
  return (
    <div className="flex flex-col gap-3 text-slate-800 lg:flex-row lg:items-center lg:justify-between">
      <div className="space-y-1">
        <div className="text-2xl font-semibold">{title}</div>
        {description && (
          <div className="text-sm text-slate-500">{description}</div>
        )}
      </div>
      {actions && <div className="flex items-center gap-2">{actions}</div>}
    </div>
  );
}
