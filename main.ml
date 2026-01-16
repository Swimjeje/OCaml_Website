(* User type and storage *)
type user = {
  mutable username: string;
  mutable password: string;
  mutable email: string;
  mutable avatar: string;
}

let users = Hashtbl.create 10

let () = 
  Hashtbl.add users "admin" {
    username = "admin";
    password = "password";
    email = "admin@example.com";
    avatar = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTMnhLfMI8-AZWBEqatrnvoCTZY9_K-awqOYw&s";
  }

let check_credentials username password =
  match Hashtbl.find_opt users username with
  | Some user -> 
    let result = user.password = password in
    Dream.log "Checking credentials: username=%s, password=%s, result=%b" username password result;
    result
  | None -> false

let get_user username =
  Hashtbl.find_opt users username

(* HTML templates *)
let login_page request error_msg =
  let csrf_token = Dream.csrf_token request in
  Dream.html @@
  Printf.sprintf {|
<!DOCTYPE html>
<html>
<head>
  <title>Login</title>
  <style>
    body {
      font-family: Arial, sans-serif;
      display: flex;
      justify-content: center;
      align-items: center;
      height: 100vh;
      margin: 0;
      background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%);
    }
    .login-container {
      background: white;
      padding: 2rem;
      border-radius: 10px;
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      width: 300px;
    }
    h1 {
      text-align: center;
      color: #333;
      margin-bottom: 1.5rem;
    }
    input {
      width: 100%%;
      padding: 0.75rem;
      margin-bottom: 1rem;
      border: 1px solid #ddd;
      border-radius: 5px;
      box-sizing: border-box;
    }
    button {
      width: 100%%;
      padding: 0.75rem;
      background: #667eea;
      color: white;
      border: none;
      border-radius: 5px;
      cursor: pointer;
      font-size: 1rem;
    }
    button:hover {
      background: #5568d3;
    }
    .error {
      color: #e53e3e;
      text-align: center;
      margin-bottom: 1rem;
    }
  </style>
</head>
<body>
  <div class="login-container">
    <h1>Login</h1>
    %s
    <form method="POST" action="/login">
      <input type="hidden" name="dream.csrf" value="%s">
      <input type="text" name="username" placeholder="Username" required>
      <input type="password" name="password" placeholder="Password" required>
      <button type="submit">Login</button>
    </form>
  </div>
</body>
</html>
|}
    (match error_msg with
     | Some msg -> Printf.sprintf {|<div class="error">%s</div>|} msg
     | None -> "")
    csrf_token

let welcome_page username =
  Dream.html @@
  Printf.sprintf {|
<!DOCTYPE html>
<html>
<head>
  <title>Welcome</title>
  <style>
    body {
      font-family: Arial, sans-serif;
      display: flex;
      justify-content: center;
      align-items: center;
      height: 100vh;
      margin: 0;
      background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%);
    }
    .welcome-container {
      background: white;
      padding: 3rem;
      border-radius: 10px;
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      text-align: center;
    }
    h1 {
      color: #333;
      margin-bottom: 1rem;
    }
    p {
      color: #666;
      margin-bottom: 2rem;
    }
    .buttons {
      display: flex;
      gap: 1rem;
      justify-content: center;
    }
    a {
      display: inline-block;
      padding: 0.75rem 2rem;
      background: #667eea;
      color: white;
      text-decoration: none;
      border-radius: 5px;
    }
    a:hover {
      background: #5568d3;
    }
    a.secondary {
      background: #48bb78;
    }
    a.secondary:hover {
      background: #38a169;
    }
  </style>
</head>
<body>
  <div class="welcome-container">
    <h1>Welcome, %s!</h1>
    <p>You have successfully logged in.</p>
    <div class="buttons">
      <a href="/profile" class="secondary">My Profile</a>
      <a href="/logout">Logout</a>
    </div>
  </div>
</body>
</html>
|} username

let profile_page user success_msg =
  Dream.html @@
  Printf.sprintf {|
<!DOCTYPE html>
<html>
<head>
  <title>Profile</title>
  <style>
    body {
      font-family: Arial, sans-serif;
      min-height: 100vh;
      margin: 0;
      background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%);
      padding: 2rem;
    }
    .profile-container {
      max-width: 600px;
      margin: 0 auto;
      background: white;
      padding: 2rem;
      border-radius: 10px;
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
    }
    h1 {
      color: #333;
      margin-bottom: 2rem;
      text-align: center;
    }
    .avatar-section {
      text-align: center;
      margin-bottom: 2rem;
    }
    .avatar {
      width: 150px;
      height: 150px;
      border-radius: 50%%;
      margin-bottom: 1rem;
    }
    .form-group {
      margin-bottom: 1.5rem;
    }
    label {
      display: block;
      color: #555;
      margin-bottom: 0.5rem;
      font-weight: bold;
    }
    input {
      width: 100%%;
      padding: 0.75rem;
      border: 1px solid #ddd;
      border-radius: 5px;
      box-sizing: border-box;
    }
    button {
      width: 100%%;
      padding: 0.75rem;
      background: #667eea;
      color: white;
      border: none;
      border-radius: 5px;
      cursor: pointer;
      font-size: 1rem;
      margin-top: 1rem;
    }
    button:hover {
      background: #5568d3;
    }
    .success {
      color: #38a169;
      text-align: center;
      margin-bottom: 1rem;
      padding: 0.75rem;
      background: #c6f6d5;
      border-radius: 5px;
    }
    .back-link {
      display: inline-block;
      margin-top: 1rem;
      color: #667eea;
      text-decoration: none;
    }
    .back-link:hover {
      text-decoration: underline;
    }
  </style>
</head>
<body>
  <div class="profile-container">
    <h1>My Profile</h1>
    %s
    <div class="avatar-section">
      <img src="%s" alt="Avatar" class="avatar">
    </div>
    <form method="POST" action="/profile/update">
      <div class="form-group">
        <label>Username</label>
        <input type="text" name="username" value="%s" required>
      </div>
      <div class="form-group">
        <label>Email</label>
        <input type="email" name="email" value="%s" required>
      </div>
      <div class="form-group">
        <label>Avatar URL</label>
        <input type="url" name="avatar" value="%s" required>
      </div>
      <div class="form-group">
        <label>New Password (leave blank to keep current)</label>
        <input type="password" name="password" placeholder="Enter new password">
      </div>
      <button type="submit">Update Profile</button>
    </form>
    <a href="/welcome" class="back-link">← Back to Welcome</a>
  </div>
</body>
</html>
|}
    (match success_msg with
     | Some msg -> Printf.sprintf {|<div class="success">%s</div>|} msg
     | None -> "")
    user.avatar
    user.username
    user.email
    user.avatar

(* Route handlers *)
let show_login_form request =
  let flash = Dream.flash_messages request in
  let error_msg = 
    match flash with
    | (_, msg) :: _ -> Some msg
    | [] -> None
  in
  login_page request error_msg

let handle_login request =
  match%lwt Dream.form ~csrf:false request with
  | `Ok fields ->
    Dream.log "Form fields received: %d fields" (List.length fields);
    List.iter (fun (k, v) -> Dream.log "  %s = %s" k v) fields;
    let username = List.assoc_opt "username" fields in
    let password = List.assoc_opt "password" fields in
    (match username, password with
     | Some u, Some p ->
       if check_credentials u p then begin
         Dream.log "Login successful for user: %s" u;
         let%lwt () = Dream.set_session_field request "user" u in
         Dream.redirect request "/welcome"
       end else begin
         Dream.log "Login failed for user: %s" u;
         Dream.add_flash_message request "Error" "Invalid username or password";
         Dream.redirect request "/"
       end
     | _ ->
       Dream.log "Missing username or password fields";
       Dream.add_flash_message request "Error" "Missing credentials";
       Dream.redirect request "/")
  | `Wrong_content_type ->
    Dream.log "Wrong content type";
    Dream.add_flash_message request "Error" "Invalid form data";
    Dream.redirect request "/"
  | _ ->
    Dream.log "Form parsing failed";
    Dream.add_flash_message request "Error" "Invalid form data";
    Dream.redirect request "/"

let show_welcome request =
  match Dream.session_field request "user" with
  | Some username -> 
    Dream.log "Welcome page accessed by: %s" username;
    welcome_page username
  | None -> 
    Dream.log "No session found, redirecting to login";
    Dream.redirect request "/"

let show_profile request =
  match Dream.session_field request "user" with
  | Some username ->
    (match get_user username with
     | Some user ->
       let flash = Dream.flash_messages request in
       let success_msg = 
         match flash with
         | (_, msg) :: _ -> Some msg
         | [] -> None
       in
       profile_page user success_msg
     | None -> Dream.redirect request "/")
  | None -> Dream.redirect request "/"

let update_profile request =
  match Dream.session_field request "user" with
  | Some old_username ->
    (match%lwt Dream.form ~csrf:false request with
     | `Ok fields ->
       Dream.log "Profile update fields received: %d fields" (List.length fields);
       let new_username = List.assoc_opt "username" fields in
       let email = List.assoc_opt "email" fields in
       let avatar = List.assoc_opt "avatar" fields in
       let password = List.assoc_opt "password" fields in
       (match get_user old_username with
        | Some user ->
          (match new_username, email, avatar with
           | Some u, Some e, Some a ->
             (* Update user data *)
             user.username <- u;
             user.email <- e;
             user.avatar <- a;
             (* Update password if provided *)
             (match password with
              | Some p when String.length p > 0 -> user.password <- p
              | _ -> ());
             (* If username changed, update hashtable and session *)
             if old_username <> u then begin
               Hashtbl.remove users old_username;
               Hashtbl.add users u user;
               let%lwt () = Dream.set_session_field request "user" u in
               Dream.log "Profile updated for user: %s" u;
               Dream.add_flash_message request "Success" "Profile updated successfully!";
               Dream.redirect request "/profile"
             end else begin
               Dream.log "Profile updated for user: %s" u;
               Dream.add_flash_message request "Success" "Profile updated successfully!";
               Dream.redirect request "/profile"
             end
           | _ ->
             Dream.add_flash_message request "Error" "Missing required fields";
             Dream.redirect request "/profile")
        | None -> Dream.redirect request "/")
     | _ ->
       Dream.add_flash_message request "Error" "Invalid form data";
       Dream.redirect request "/profile")
  | None -> Dream.redirect request "/"

let handle_logout request =
  let%lwt () = Dream.invalidate_session request in
  Dream.redirect request "/"

(* Main application *)
let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.flash
  @@ Dream.router [
    Dream.get  "/"               show_login_form;
    Dream.post "/login"          handle_login;
    Dream.get  "/welcome"        show_welcome;
    Dream.get  "/profile"        show_profile;
    Dream.post "/profile/update" update_profile;
    Dream.get  "/logout"         handle_logout;
  ]