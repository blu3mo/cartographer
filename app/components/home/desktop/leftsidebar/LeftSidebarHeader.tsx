"use client";

import type { ReactNode } from "react";

type LeftSidebarHeaderProps = {
  toolbar?: ReactNode;
  title?: ReactNode;
  description?: ReactNode;
};

export function LeftSidebarHeader({
  toolbar,
  title,
  description,
}: LeftSidebarHeaderProps) {
  return (
    <div className="flex flex-col gap-3 text-slate-100">
      {title && <div className="text-lg font-semibold">{title}</div>}
      {description && (
        <div className="text-xs font-medium text-slate-400">{description}</div>
      )}
      {toolbar && <div className="flex items-center gap-3">{toolbar}</div>}
    </div>
  );
}
