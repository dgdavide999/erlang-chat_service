FROM erlang:25
WORKDIR /app

# Install Rebar3
RUN wget https://github.com/erlang/rebar3/releases/download/3.16.0/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/

# Copy everything in after setting up rebar3
COPY . .

# Fetch deps and compile the project
RUN rebar3 get-deps && rebar3 compile

# (not required for eunit)
# RUN rebar3 release

# Run EUnit tests when the container starts
CMD ["rebar3", "eunit"]
