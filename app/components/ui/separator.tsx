import * as React from "react";

import { cn } from "@/lib/utils";

const Separator = React.forwardRef<
  HTMLDivElement,
  React.HTMLAttributes<HTMLDivElement> & {
    orientation?: "horizontal" | "vertical";
  }
>(({ className, orientation = "horizontal", ...props }, ref) => {
  const classes = cn(
    "shrink-0 bg-border",
    orientation === "horizontal" ? "h-[1px] w-full" : "h-full w-[1px]",
    className,
  );

  return <div ref={ref} aria-hidden="true" className={classes} {...props} />;
});
Separator.displayName = "Separator";

export { Separator };
