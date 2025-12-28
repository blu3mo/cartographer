import { clsx } from "clsx";
import { motion } from "framer-motion";

interface SkeletonProps {
  className?: string;
  variant?: "text" | "circular" | "rectangular";
}

export function Skeleton({ className, variant = "text" }: SkeletonProps) {
  return (
    <motion.div
      initial={{ opacity: 0.6 }}
      animate={{ opacity: 1 }}
      transition={{
        repeat: Infinity,
        repeatType: "reverse",
        duration: 0.8,
        ease: "easeInOut",
      }}
      className={clsx(
        "bg-gray-200",
        {
          "rounded-full": variant === "circular",
          "rounded-md": variant === "rectangular",
          rounded: variant === "text",
        },
        className,
      )}
    />
  );
}

export function StatementSkeleton() {
  return (
    <div className="bg-white/50 backdrop-blur-sm rounded-2xl p-6 border border-gray-100">
      <Skeleton className="h-6 w-3/4 mb-4" />
      <Skeleton className="h-6 w-full mb-2" />
      <Skeleton className="h-6 w-5/6" />
    </div>
  );
}

export function CardSkeleton() {
  return (
    <div className="bg-white rounded-xl border border-gray-100 p-5 animate-pulse">
      <Skeleton className="h-5 w-2/3 mb-3" />
      <Skeleton className="h-4 w-full mb-2" />
      <Skeleton className="h-4 w-4/5 mb-4" />
      <div className="flex items-center justify-between">
        <div className="flex gap-3">
          <Skeleton className="h-3 w-16" />
          <Skeleton className="h-3 w-12" />
        </div>
        <Skeleton className="h-8 w-20" />
      </div>
    </div>
  );
}
