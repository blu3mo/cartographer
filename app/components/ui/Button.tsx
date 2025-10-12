import { motion } from 'framer-motion';
import { clsx } from 'clsx';
import { Loader2 } from 'lucide-react';

interface ButtonProps extends React.ButtonHTMLAttributes<HTMLButtonElement> {
  variant?: 'primary' | 'secondary' | 'ghost' | 'answer';
  size?: 'sm' | 'md' | 'lg';
  isLoading?: boolean;
  children: React.ReactNode;
}

export function Button({
  variant = 'primary',
  size = 'md',
  isLoading = false,
  children,
  className,
  disabled,
  ...props
}: ButtonProps) {
  return (
    <motion.button
      whileHover={{ scale: disabled || isLoading ? 1 : 1.02 }}
      whileTap={{ scale: disabled || isLoading ? 1 : 0.98 }}
      disabled={disabled || isLoading}
      className={clsx(
        'inline-flex items-center justify-center font-medium rounded-xl transition-all duration-200',
        'focus:outline-none focus:ring-2 focus:ring-offset-2',
        'disabled:opacity-50 disabled:cursor-not-allowed',
        {
          // Primary variant (main CTA)
          'bg-gray-900 text-white hover:bg-gray-800 focus:ring-gray-900 shadow-sm':
            variant === 'primary',

          // Secondary variant
          'bg-white text-gray-900 border border-gray-200 hover:bg-gray-50 focus:ring-gray-900 shadow-sm':
            variant === 'secondary',

          // Ghost variant
          'bg-transparent text-gray-700 hover:bg-gray-50 focus:ring-gray-300':
            variant === 'ghost',

          // Answer variant (for participant answers)
          'bg-white text-gray-900 border-2 border-gray-200 hover:border-gray-900 focus:ring-gray-900 shadow-sm font-semibold':
            variant === 'answer',

          // Sizes
          'px-3 py-1.5 text-sm': size === 'sm',
          'px-5 py-2.5 text-base': size === 'md',
          'px-6 py-3.5 text-lg': size === 'lg',
        },
        className
      )}
      {...props}
    >
      {isLoading && <Loader2 className="w-4 h-4 mr-2 animate-spin" />}
      {children}
    </motion.button>
  );
}
