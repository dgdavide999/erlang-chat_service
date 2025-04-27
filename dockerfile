FROM erlang:25
WORKDIR /app

# Install Rebar3
RUN wget -qO /usr/local/bin/rebar3 \
    https://github.com/erlang/rebar3/releases/download/3.16.0/rebar3 && \
    chmod +x /usr/local/bin/rebar3

# Copy files
COPY rebar.config ./
COPY src ./src
COPY test ./test

# Fetch dependencies, compile, and build the release
RUN rebar3 get-deps
RUN rebar3 as test compile
RUN rebar3 release

# Expose your TCP port
EXPOSE 8081

# Run the release in foreground
CMD ["_build/default/rel/miniclip_test/bin/miniclip_test", "foreground"]
