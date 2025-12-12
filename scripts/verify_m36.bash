#!/usr/bin/env bash
set -e

# Configuration
PORT="6543"
HOST="localhost"

echo "Waiting for Project M36 server..."
until nc -z $HOST $PORT; do
  sleep 1
done
echo "Project M36 server is up."

echo "Verifying Schema..."
RESULT=$(tutd -p $PORT -h $HOST -e ":showexpr events" | grep "events")
if [[ -z "$RESULT" ]]; then
  echo "FAIL: 'events' relvar not found."
fi

echo "Test 1: Insert Valid UserCreatedV1 Event"
# Using ADT constructors: EventId(UUID), StreamId(UUID), UserId(UUID), TraceContext(...)
UUID_1="10000000-0000-0000-0000-000000000001"
UUID_STREAM="20000000-0000-0000-0000-000000000001"
# StreamCategory: UserStream
# Payload: UserCreatedV1 "Alice" 30
# Trace: TraceContext Nothing Nothing Nothing
NOW=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

tutd -p $PORT -h $HOST -e ":insert events {tuple {id (EventId \"$UUID_1\"), stream_id (StreamId \"$UUID_STREAM\"), category UserStream, version 1, payload (UserCreatedV1 \"Alice\" 30), trace (TraceContext Nothing Nothing Nothing), occurred_at \"$NOW\"}}"

echo "Test 1 Passed: Insert successful."

echo "Test 2: Insert Validation Failure (Type Mismatch)"
# Try UserCreatedV1 with String age ("Thirty") -> Generic Type Error
if tutd -p $PORT -h $HOST -e ":insert events {tuple {id (EventId \"$UUID_1\"), stream_id (StreamId \"$UUID_STREAM\"), category UserStream, version 2, payload (UserCreatedV1 \"Alice\" \"Thirty\"), trace (TraceContext Nothing Nothing Nothing), occurred_at \"$NOW\"}}" 2>/dev/null; then
   echo "FAIL: Test 2 should have failed but succeeded."
   exit 1
else
   echo "Test 2 Passed: Invalid payload rejected."
fi

echo "ALL TESTS PASSED"
