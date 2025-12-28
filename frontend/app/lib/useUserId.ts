"use client";

import { useEffect, useState } from "react";

const USER_ID_KEY = "cartographer_user_id";

function generateUserId(): string {
  return crypto.randomUUID();
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
