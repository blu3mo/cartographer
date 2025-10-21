import { NextResponse } from 'next/server';
import type { NextRequest } from 'next/server';

const ipAllowlist = (process.env.ALLOWED_IPS ?? '')
  .split(',')
  .map((ip) => ip.trim())
  .filter(Boolean);

const isIpRestrictionEnabled = ipAllowlist.length > 0;

const basicAuthUsername = process.env.BASIC_AUTH_USERNAME ?? '';
const basicAuthPassword = process.env.BASIC_AUTH_PASSWORD ?? '';
const basicAuthRealm = process.env.BASIC_AUTH_REALM ?? 'Restricted';
const isBasicAuthEnabled = Boolean(basicAuthUsername && basicAuthPassword);

const BASIC_AUTH_COOKIE_NAME = 'basic-auth-session';

let cachedBasicCookieValue: string | null = null;

export async function middleware(request: NextRequest) {
  if (request.method === 'OPTIONS') {
    return NextResponse.next();
  }

  if (isIpRestrictionEnabled) {
    const ipResponse = enforceIpAllowlist(request);
    if (ipResponse) {
      return ipResponse;
    }
  }

  if (isBasicAuthEnabled) {
    const basicResponse = await enforceBasicAuth(request);
    if (basicResponse) {
      return basicResponse;
    }
  }

  return NextResponse.next();
}

function enforceIpAllowlist(request: NextRequest): NextResponse | undefined {
  const clientIp = getClientIp(request);
  if (!clientIp) {
    return undefined;
  }

  const isAllowed = ipAllowlist.some((allowedIp) => allowedIp === clientIp);
  if (isAllowed) {
    return undefined;
  }

  return new NextResponse('Forbidden', { status: 403 });
}

async function enforceBasicAuth(request: NextRequest): Promise<NextResponse | undefined> {
  const expectedCookieValue = await getBasicCookieValue();
  const existingCookie = request.cookies.get(BASIC_AUTH_COOKIE_NAME);

  if (existingCookie?.value === expectedCookieValue) {
    return undefined;
  }

  const header = request.headers.get('authorization');
  if (!header || !header.startsWith('Basic ')) {
    return unauthorizedResponse();
  }

  const encodedCredentials = header.slice(6).trim();
  const decodedCredentials = decodeBase64(encodedCredentials);
  if (!decodedCredentials) {
    return unauthorizedResponse();
  }

  const separatorIndex = decodedCredentials.indexOf(':');
  if (separatorIndex === -1) {
    return unauthorizedResponse();
  }

  const username = decodedCredentials.slice(0, separatorIndex);
  const password = decodedCredentials.slice(separatorIndex + 1);

  if (username !== basicAuthUsername || password !== basicAuthPassword) {
    return unauthorizedResponse();
  }

  const response = NextResponse.next();
  response.cookies.set({
    name: BASIC_AUTH_COOKIE_NAME,
    value: expectedCookieValue,
    httpOnly: true,
    sameSite: 'lax',
    secure: process.env.NODE_ENV === 'production',
    path: '/',
  });

  return response;
}

function decodeBase64(value: string): string {
  try {
    return atob(value);
  } catch {
    return '';
  }
}

async function getBasicCookieValue(): Promise<string> {
  if (cachedBasicCookieValue) {
    return cachedBasicCookieValue;
  }

  const rawValue = `${basicAuthUsername}:${basicAuthPassword}`;
  cachedBasicCookieValue = await sha256Hex(rawValue);
  return cachedBasicCookieValue;
}

async function sha256Hex(value: string): Promise<string> {
  const encoder = new TextEncoder();
  const data = encoder.encode(value);
  const digest = await crypto.subtle.digest('SHA-256', data);
  const hashArray = Array.from(new Uint8Array(digest));
  return hashArray.map((byte) => byte.toString(16).padStart(2, '0')).join('');
}

function getClientIp(request: NextRequest): string | null {
  const forwarded = request.headers.get('x-forwarded-for');
  if (forwarded) {
    const [first] = forwarded.split(',');
    if (first && first.trim()) {
      return first.trim();
    }
  }

  const fallbackHeaders = [
    'x-real-ip',
    'x-client-ip',
    'cf-connecting-ip',
    'true-client-ip',
    'x-cluster-client-ip',
  ];

  for (const header of fallbackHeaders) {
    const value = request.headers.get(header);
    if (value && value.trim()) {
      return value.trim();
    }
  }

  return null;
}

function unauthorizedResponse(): NextResponse {
  return new NextResponse('Unauthorized', {
    status: 401,
    headers: {
      'WWW-Authenticate': `Basic realm="${basicAuthRealm}"`,
    },
  });
}
