import { type ClassValue, clsx } from "clsx";
import { twMerge } from "tailwind-merge";

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}

export const toAbsoluteUrl = (path: string) =>
  typeof window === "undefined"
    ? path
    : /^https?:\/\//i.test(path)
      ? path
      : `${window.location.origin}${path.startsWith("/") ? path : `/${path}`}`;