"use client";

import { Loader2 } from "lucide-react";
import { useRouter } from "next/navigation";
import { useEffect } from "react";

import { AppHeader } from "@/components/AppHeader";
import { CreateSessionForm } from "@/components/sessions/CreateSessionForm";
import {
  Breadcrumb,
  BreadcrumbItem,
  BreadcrumbLink,
  BreadcrumbList,
  BreadcrumbPage,
  BreadcrumbSeparator,
} from "@/components/ui/breadcrumb";
import { useUserId } from "@/lib/useUserId";

export default function NewSessionPage() {
  const router = useRouter();
  const { userId, isLoading: userLoading } = useUserId();

  useEffect(() => {
    document.title = "新しいセッションを作成 - Cartographer";
    return () => {
      document.title = "Cartographer - 認識を可視化し、合意形成を促進する";
    };
  }, []);

  if (userLoading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-background">
      <AppHeader />
      <div className="max-w-2xl mx-auto px-4 py-12 sm:px-6 lg:px-8">
        <Breadcrumb className="mb-6">
          <BreadcrumbList>
            <BreadcrumbItem>
              <BreadcrumbLink href="/">ホーム</BreadcrumbLink>
            </BreadcrumbItem>
            <BreadcrumbSeparator />
            <BreadcrumbItem>
              <BreadcrumbPage>セッション作成</BreadcrumbPage>
            </BreadcrumbItem>
          </BreadcrumbList>
        </Breadcrumb>
        <div className="mb-8">
          <h1 className="text-3xl font-bold tracking-tight mb-2">
            新しいセッションを作成
          </h1>
          <p className="text-muted-foreground">
            入力した「セッション情報」を元に、AIがさまざまな角度からの『質問』を生成します
          </p>
        </div>

        <CreateSessionForm
          userId={userId}
          onSuccess={async (session) => {
            router.push(`/?sessionId=${session.id}`);
          }}
        />
      </div>
    </div>
  );
}
