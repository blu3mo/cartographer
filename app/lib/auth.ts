import { NextRequest } from 'next/server';

export function getUserIdFromRequest(request: NextRequest): string | null {
  const authHeader = request.headers.get('authorization');
  if (!authHeader || !authHeader.startsWith('Bearer ')) {
    return null;
  }
  return authHeader.substring(7);
}

export function createAuthorizationHeader(userId: string): Record<string, string> {
  return {
    'Authorization': `Bearer ${userId}`,
  };
}
