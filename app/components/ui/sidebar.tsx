"use client";

import * as React from "react";
import { Menu } from "lucide-react";

import { cn } from "@/lib/utils";
import { Button } from "@/components/ui/Button";

type SidebarContextValue = {
  open: boolean;
  toggle: () => void;
  setOpen: (value: boolean) => void;
};

const SidebarContext = React.createContext<SidebarContextValue | null>(null);

type SidebarProviderProps = {
  children: React.ReactNode;
  defaultOpen?: boolean;
};

export function SidebarProvider({
  children,
  defaultOpen = false,
}: SidebarProviderProps) {
  const [open, setOpen] = React.useState(defaultOpen);
  const toggle = React.useCallback(() => {
    setOpen((previous) => !previous);
  }, []);

  const value = React.useMemo(
    () => ({ open, toggle, setOpen }),
    [open, toggle],
  );

  return (
    <SidebarContext.Provider value={value}>
      {children}
    </SidebarContext.Provider>
  );
}

export function useSidebar() {
  const context = React.useContext(SidebarContext);
  if (!context) {
    throw new Error("useSidebar must be used within a SidebarProvider");
  }

  return context;
}

type SidebarProps = React.HTMLAttributes<HTMLElement>;

export const Sidebar = React.forwardRef<HTMLElement, SidebarProps>(
  ({ className, children, ...props }, ref) => {
    const { open } = useSidebar();
    return (
      <aside
        ref={ref}
        data-state={open ? "open" : "closed"}
        className={cn(
          "fixed inset-y-0 left-0 z-40 flex w-72 flex-col border-r border-[var(--sidebar-border)] bg-[var(--sidebar)] text-[var(--sidebar-foreground)] transition-transform duration-200 ease-in-out lg:static lg:translate-x-0",
          open ? "translate-x-0" : "-translate-x-full",
          className,
        )}
        {...props}
      >
        {children}
      </aside>
    );
  },
);
Sidebar.displayName = "Sidebar";

export function SidebarOverlay() {
  const { open, toggle } = useSidebar();
  return (
    <div
      aria-hidden="true"
      className={cn(
        "fixed inset-0 z-30 bg-black/40 transition-opacity lg:hidden",
        open ? "pointer-events-auto opacity-100" : "pointer-events-none opacity-0",
      )}
      onClick={toggle}
    />
  );
}

type SidebarSectionProps = React.HTMLAttributes<HTMLDivElement>;

export function SidebarHeader({ className, ...props }: SidebarSectionProps) {
  return (
    <div
      className={cn(
        "border-b border-[var(--sidebar-border)] px-4 py-4",
        className,
      )}
      {...props}
    />
  );
}

export function SidebarFooter({ className, ...props }: SidebarSectionProps) {
  return (
    <div
      className={cn(
        "mt-auto border-t border-[var(--sidebar-border)] px-4 py-4",
        className,
      )}
      {...props}
    />
  );
}

export function SidebarContent({ className, ...props }: SidebarSectionProps) {
  return (
    <div
      className={cn("flex-1 overflow-y-auto px-3 py-4", className)}
      {...props}
    />
  );
}

export function SidebarGroup({ className, ...props }: SidebarSectionProps) {
  return (
    <div
      className={cn(
        "space-y-2 rounded-2xl bg-[var(--sidebar-accent)]/60 p-3",
        className,
      )}
      {...props}
    />
  );
}

export function SidebarGroupLabel({
  className,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) {
  return (
    <div
      className={cn(
        "flex items-center justify-between text-xs font-semibold uppercase tracking-wide text-[var(--sidebar-foreground)]/70",
        className,
      )}
      {...props}
    />
  );
}

export function SidebarGroupContent({
  className,
  ...props
}: SidebarSectionProps) {
  return <div className={cn("space-y-1", className)} {...props} />;
}

export function SidebarMenu({
  className,
  ...props
}: React.HTMLAttributes<HTMLUListElement>) {
  return <ul className={cn("space-y-1", className)} {...props} />;
}

export function SidebarMenuItem({
  className,
  ...props
}: React.LiHTMLAttributes<HTMLLIElement>) {
  return <li className={cn("list-none", className)} {...props} />;
}

type SidebarMenuButtonProps = React.ButtonHTMLAttributes<HTMLButtonElement> & {
  isActive?: boolean;
  icon?: React.ReactNode;
};

export function SidebarMenuButton({
  className,
  children,
  isActive = false,
  icon,
  ...props
}: SidebarMenuButtonProps) {
  return (
    <button
      type="button"
      className={cn(
        "flex w-full items-center gap-2 rounded-xl px-3 py-2 text-sm font-medium transition-colors focus:outline-none",
        isActive
          ? "bg-[var(--sidebar-primary)] text-[var(--sidebar-primary-foreground)] shadow"
          : "text-[var(--sidebar-foreground)] hover:bg-[var(--sidebar-accent)]",
        className,
      )}
      {...props}
    >
      {icon && <span className="text-base">{icon}</span>}
      <span className="truncate">{children}</span>
    </button>
  );
}

export function SidebarTrigger({
  className,
  ...props
}: React.ButtonHTMLAttributes<HTMLButtonElement>) {
  const { toggle } = useSidebar();

  return (
    <Button
      type="button"
      variant="outline"
      size="icon"
      className={cn("lg:hidden", className)}
      onClick={toggle}
      {...props}
    >
      <Menu className="h-5 w-5" />
      <span className="sr-only">Toggle sidebar</span>
    </Button>
  );
}

type SidebarInsetProps = React.HTMLAttributes<HTMLDivElement>;

export function SidebarInset({ className, ...props }: SidebarInsetProps) {
  return (
    <div
      className={cn(
        "flex min-h-screen w-full flex-1 flex-col bg-background lg:ml-72",
        className,
      )}
      {...props}
    />
  );
}
