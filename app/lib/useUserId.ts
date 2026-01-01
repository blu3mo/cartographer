"use client";

import { useEffect, useState } from "react";

const USER_ID_KEY = "cartographer_user_id";

function generateUserId(): string {
  if (typeof crypto !== "undefined" && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  // Fallback for non-secure contexts (http)
  return "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx".replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === "x" ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

export function useUserId() {
  const [userId, setUserId] = useState<string | null>(null);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    // Get or create user ID
    let id = localStorage.getItem(USER_ID_KEY);
    if (!id) {
      id = generateUserId();
      localStorage.setItem(USER_ID_KEY, id);
    }
    setUserId(id);
    setIsLoading(false);
  }, []);

  return { userId, isLoading };
}

export function getUserId(): string | null {
  if (typeof window === "undefined") return null;
  return localStorage.getItem(USER_ID_KEY);
}
