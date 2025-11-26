#!/bin/bash

# Start the server in the background
echo "Starting server..."
export DYLD_INSERT_LIBRARIES=/usr/local/lib/libsecp256k1.dylib
./_build/default/bin/main.exe &
SERVER_PID=$!

# Wait for server to start
sleep 2

echo "Testing PUT /upload without Auth..."
curl -v -X PUT --data "test" http://localhost:8082/upload 2>&1 | grep "401 Unauthorized"
curl -v -X PUT --data "test" http://localhost:8082/upload 2>&1 | grep "X-Reason: Missing Authorization header"


# Cleanup
kill $SERVER_PID
