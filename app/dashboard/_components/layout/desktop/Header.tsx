"use client";

import type { ReactNode } from "react";

type HeaderProps = {
  children: ReactNode;
};

export function Header({ children }: HeaderProps) {
  return (
    <header className="flex h-[146px] items-center justify-between gap-6 border-b border-slate-200 bg-slate-50 px-8 py-6">
      {children}
    </header>
  );
}
