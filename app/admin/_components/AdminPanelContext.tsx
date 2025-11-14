"use client";

import {
  createContext,
  useContext,
  useMemo,
  useState,
  type ReactNode,
} from "react";

type AdminPanelContextValue = {
  panelContainer: HTMLElement | null;
  setPanelContainer: (element: HTMLElement | null) => void;
};

const AdminPanelContext = createContext<AdminPanelContextValue | null>(null);

export function AdminPanelProvider({ children }: { children: ReactNode }) {
  const [panelContainer, setPanelContainer] = useState<HTMLElement | null>(null);
  const value = useMemo(
    () => ({ panelContainer, setPanelContainer }),
    [panelContainer],
  );

  return (
    <AdminPanelContext.Provider value={value}>
      {children}
    </AdminPanelContext.Provider>
  );
}

export function useAdminPanel() {
  const context = useContext(AdminPanelContext);
  if (!context) {
    throw new Error("useAdminPanel must be used within an AdminPanelProvider");
  }
  return context;
}

export function useAdminPanelContext() {
  return useContext(AdminPanelContext);
}
