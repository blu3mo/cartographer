"use client";

import { ChevronLeft, ChevronRight, Info, X } from "lucide-react";
import { useState } from "react";

import { cn } from "@/lib/utils";

type AboutSlideContent = {
  title: string;
  description: string;
  details?: string[];
  bullets?: string[];
};

const ABOUT_SLIDES: AboutSlideContent[] = [
  {
    title: "Cartographerとは",
    description:
      "Cartographerとは、「それぞれの認識を洗い出し、合意点、相違点、不明点を可視化するツール」です。",
    details: [
      "合意形成や意思決定の現場で、認識ギャップを素早く把握するためのファシリテーション支援を目的に設計されています。",
    ],
  },
  {
    title: "どんな時に活用できるか",
    description: "以下のような場面で力を発揮します。",
    bullets: [
      "チーム内で目標や成果物イメージがズレており、素早く認識を合わせたいとき",
      "行政・企業・コミュニティなど多様な価値観が混在する場面で、合意できるポイントを見つけたいとき",
      "「誰も答えられない重要な問い」を掘り起こし、次に検証すべき仮説を定めたいワークショップ／ヒアリング",
    ],
  },
  {
    title: "実際のユースケース",
    description: "現場では次のように活用されています。",
    bullets: [
      "千人規模の企業や自治体で、部門間のゴールのズレを洗い出すセッション",
      "市民・コミュニティヒアリングで「個人の欲求」と「共同体としての方針」を切り分けて整理する場面",
      "クライアントと自社で成果物イメージが食い違うときに、期待値の差分を可視化して再調整するケース",
    ],
  },
  {
    title: "利用方法（実務的な流れ・ポイント）",
    description:
      "AIが生成した質問・レポートによって生まれる議論を元に、次のステップで運用することを推奨します:",
    bullets: [
      "セッションの目的と洗い出したい認識を定義し、タイトル／ゴール／背景コンテキストを準備する",
      "セッション内容に関連しそうなチャットログや、ドキュメントなどを「参考情報」に貼り付ける",
      "Cartographerを使って起こった議論を録音・文字起こしし、再度関連情報として入力する",
    ],
  },
] as const;

type AboutCartographerButtonProps = {
  className?: string;
};

export function AboutCartographerButton({
  className,
}: AboutCartographerButtonProps) {
  const [isAboutOpen, setIsAboutOpen] = useState(false);
  const [aboutPageIndex, setAboutPageIndex] = useState(0);
  const openAboutModal = () => {
    setAboutPageIndex(0);
    setIsAboutOpen(true);
  };
  const closeAboutModal = () => {
    setIsAboutOpen(false);
  };
  const currentSlide = ABOUT_SLIDES[aboutPageIndex];

  return (
    <>
      <button
        type="button"
        onClick={openAboutModal}
        className={cn(
          "inline-flex items-center gap-1 text-sm font-medium text-slate-600 transition-colors hover:text-slate-900",
          className,
        )}
      >
        <Info className="h-4 w-4" aria-hidden="true" />
        Cartographerについて
      </button>
      {isAboutOpen && (
        <div className="fixed inset-0 z-50 flex items-center justify-center px-4 py-8">
          <button
            type="button"
            aria-label="モーダルを閉じる"
            className="absolute inset-0 h-full w-full bg-slate-950/50"
            onClick={closeAboutModal}
          />
          <div
            className="relative max-w-2xl w-full rounded-3xl border border-slate-200 bg-white p-6 shadow-2xl"
            role="dialog"
            aria-modal="true"
          >
            <div className="flex items-center justify-between gap-4">
              <div>
                <h2 className="text-lg font-semibold text-slate-900">
                  {currentSlide.title}
                </h2>
              </div>
              <button
                type="button"
                onClick={closeAboutModal}
                aria-label="閉じる"
                className="rounded-full border border-slate-200 p-2 text-slate-500 transition-colors hover:border-slate-300 hover:text-slate-900"
              >
                <X className="h-4 w-4" />
              </button>
            </div>
            <AboutSlide slide={currentSlide} />
            <div className="mt-6 flex items-center justify-between text-sm">
              <button
                type="button"
                onClick={() =>
                  setAboutPageIndex((prev) => Math.max(0, prev - 1))
                }
                disabled={aboutPageIndex === 0}
                className="flex items-center gap-2 rounded-full border border-slate-200 px-4 py-2 font-medium text-slate-600 transition-colors hover:text-slate-900 disabled:cursor-not-allowed disabled:opacity-50"
              >
                <ChevronLeft className="h-4 w-4" />
                前へ
              </button>
              <div className="text-xs font-medium text-slate-500">
                {aboutPageIndex + 1} / {ABOUT_SLIDES.length}
              </div>
              {aboutPageIndex === ABOUT_SLIDES.length - 1 ? (
                <button
                  type="button"
                  onClick={closeAboutModal}
                  className="rounded-full border border-slate-900 bg-slate-900 px-4 py-2 font-medium text-white transition-colors hover:bg-slate-800"
                >
                  閉じる
                </button>
              ) : (
                <button
                  type="button"
                  onClick={() =>
                    setAboutPageIndex((prev) =>
                      Math.min(ABOUT_SLIDES.length - 1, prev + 1),
                    )
                  }
                  className="flex items-center gap-2 rounded-full border border-slate-900 bg-slate-900 px-4 py-2 font-medium text-white transition-colors hover:bg-slate-800"
                >
                  次へ
                  <ChevronRight className="h-4 w-4" />
                </button>
              )}
            </div>
          </div>
        </div>
      )}
    </>
  );
}

type AboutSlideProps = {
  slide: AboutSlideContent;
};

function AboutSlide({ slide }: AboutSlideProps) {
  return (
    <div className="mt-6 space-y-4 rounded-2xl bg-slate-50/70 px-5 py-4 text-sm leading-relaxed text-slate-700">
      <div>
        <p className="mt-2 text-slate-600">{slide.description}</p>
      </div>
      {slide.details?.map((detail) => (
        <p key={detail} className="text-slate-600">
          {detail}
        </p>
      ))}
      {slide.bullets && (
        <ul className="list-disc space-y-2 pl-6 text-slate-700">
          {slide.bullets.map((bullet) => (
            <li key={bullet}>{bullet}</li>
          ))}
        </ul>
      )}
    </div>
  );
}
