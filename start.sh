erl -pa ebin -pa deps/*/ebin -config etc/app.config -boot start_sasl -s reloader -s enodeman_app -name enodeman@127.0.0.1
