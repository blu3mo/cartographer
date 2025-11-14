"use client";

import type { ReactNode } from "react";

type SessionProps = {
  children: ReactNode;
};

export function Session({ children }: SessionProps) {
  return <div className="grid gap-5 text-sm text-slate-200">{children}</div>;
}
