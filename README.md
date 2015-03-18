EditorEngine.Emacs
====================

Emacs plugin for the Editor Engine and OpenIDE

Protocol

Incoming messages
Message                                                 Response string
-----------------------------------------------------------------------
ping                                                    pong
can-insert-for                                          true if editor supports insert, false if not
can-remove-for                                          true if editor supports remove, false if not
get-dirty-buffers                                       FILENAME|FILENAME|FILENAME|...
get-buffer-content                                      Return file content with newline replaced with ||newline||
caret                                                   FILENAME|LINE|COLUMN
get-windows                                             WINDOW1|WINDOW2|WINDOW3|...

Message                                                 Action
--------------------------------------------------------------
goto FILE LINE COLUMN [WINDOWID]                        Go to already opened file or open in current buffer if not a window is specified
insert TEXT FILENAME LINE COLUMN                        Replace ||newline|| with newline then insert text at given position. If file is not open then open it in current buffer
remove FILENAME LINE-START COL-START LINE-END COL-END   Remove text for given range. If file is not open then open it in current buffer
user-select TOKEN COMMA-SEPARATED-LIST                  Display a list where the user can select an item and filter the list by typing.
  -- if items contains || split and treat them as key value pair
  -- token is any value chosen by sender of message
  -- if user cancels post string: user-selected "TOKEN" "user-cancelled"
  -- if user chooses a value post string: user-selected "TOKEN" "KEY"
user-select-at-caret TOKEN COMMA-SEPARATED-LIST         Display a list at caret where the user can select an item.
  -- if items contains || split and treat them as key value pair
  -- token is any value chosen by sender of message
  -- if user cancels post string: user-selected-at-caret "TOKEN" "user-cancelled"
  -- if user chooses a value post string: user-selected-at-caret "TOKEN" "KEY"
user-input TOKEN DEFAULT-VALUE                          Give the user an option to enter a line of text. Prefill with default value
  -- if user cancels post string: user-inputted "TOKEN" "user-cancelled"
  -- if user enters a value post string: user-inputted "TOKEN" "VALUE"

Editor Events
Action                                                  Response string
-----------------------------------------------------------------------
Buffer changed                                          editor buffer-changed /path/to/buffer/file

Commands
When sending commands out use the OpenIDE session token located closest
to the the file opened in the current buffer. If no file in current
buffer use current directory
Type of Command                                         Command syntax
----------------------------------------------------------------------
General command                                         command COMMAND-STRING
Language command where default language is none         FILE-EXTENSION command COMMAND-STRING


Locate tokens
---------------------
Tokens are located in an EditorEngine directory in the default temp directory ie on linux it's /tmp
The file name will be the process id and the extension .pid. Inside you will find the token printed
on line one and line 2 will have the tcp port to use. Common usage is when failing to connect to
the port to delete the file.


Get nearest config point (python)
------------------------
def get_nearest_config_point(file_name):
    path = file_name
    while True:
        if path == os.path.dirname(path) :
            break
        path = os.path.dirname(path) 
        config_point = os.path.join(path, ".OpenIDE")
        if os.path.exists(config_point):
            return path;
    return None

Get nearest token
-----------------
def get_editor_engine_token(file_name):
    tempdir = tempfile.gettempdir()
    if sys.platform == "darwin":
        tempdir = "/tmp"
    editor_token_path = os.path.join(tempdir, "EditorEngine")
    engines = []
    if os.path.exists(editor_token_path) == True:
        all_engines = []
        for pid_file_name in os.listdir(editor_token_path):
            pid_file = os.path.join(editor_token_path, pid_file_name)
            if pid_file.endswith(".pid"):
				# Get token path and tcp port
                client = get_editor_engine_client_settings(pid_file)
                if client != None:
                    all_engines.append(client)
                    if file_name != None:
						# If the file name contains the token path add
						# it to matching engines
                        if is_same_as_engine(client[0], file_name):
                            engines.append(client)
		# If none really matched pick first alive token
        if len(engines) == 0 and len(all_engines) > 0:
            return all_engines[0]
	# Pick nearest matching token
    return get_nearest_token(engines)

def is_same_as_engine(running, suggested):
    return running == suggested or suggested.startswith(running+os.sep)

def get_nearest_token(engines):
    if len(engines) == 0:
        return None
    closest_client = None
    for client in engines:
        if closest_client == None:
            closest_client = client
            continue

		# Compare length of tokens
        if len(client[0]) > len(closest_client[0]):
            closest_client = client
    return closest_client

def get_editor_engine_client_settings(pid_file):
    try:
        with open(pid_file) as f:
            lines = f.readlines()
            if len(lines) != 2:
                return None
            client = []
			# Token path
            client.append(lines[0].replace("\n", ""))
			# Token tcp port
            client.append(int(lines[1].replace("\n", "")))
			# If we can connect to socket the token is alive
            sock = get_editor_engine_socket_client(client)
            sock.close()
            return client
    except:
		# If the token is dead remove the file
        os.remove(pid_file)
        return None
    return None

def get_editor_engine_socket_client(client):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(("localhost", client[1]))
    return sock
