-- CreateEnum
-- No enums to create for this migration

-- AlterTable
ALTER TABLE "sessions" ADD COLUMN "is_public" BOOLEAN NOT NULL DEFAULT true;
