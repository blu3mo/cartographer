"use client";

type SessionHighlightStatProps = {
  label: string;
  value: string | number;
};

export function SessionHighlightStat({
  label,
  value,
}: SessionHighlightStatProps) {
  return (
    <div className="rounded-2xl border border-slate-100 bg-slate-50 px-3 py-2">
      <p className="text-[10px] font-semibold uppercase tracking-wide text-slate-500">
        {label}
      </p>
      <p className="text-lg font-bold text-slate-900">{value}</p>
    </div>
  );
}

type SessionMetricListProps = {
  metrics: Array<SessionHighlightStatProps>;
  className?: string;
};

export function SessionMetricGrid({
  metrics,
  className = "",
}: SessionMetricListProps) {
  if (metrics.length === 0) return null;
  return (
    <div className={`grid gap-3 ${className}`.trim()}>
      {metrics.map((metric) => (
        <SessionHighlightStat
          key={metric.label}
          label={metric.label}
          value={metric.value}
        />
      ))}
    </div>
  );
}
