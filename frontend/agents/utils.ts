export const delay = (ms: number) =>
  new Promise((resolve) => {
    setTimeout(resolve, ms);
  });

export const truncate = (value: string, length = 8) => {
  if (value.length <= length) {
    return value;
  }
  return `${value.slice(0, length / 2)}…${value.slice(-length / 2)}`;
};
