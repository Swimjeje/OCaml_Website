(* User type and storage *)
type user = {
  mutable username: string;
  mutable password: string;
  mutable email: string;
  mutable avatar: string;
}

(* Price item type and storage *)
type price_item = {
  id: string;
  mutable name: string;
  mutable price: float;
  mutable category: string;
}

let users = Hashtbl.create 10
let prices = Hashtbl.create 20

let () = 
  Hashtbl.add users "admin" {
    username = "admin";
    password = "password";
    email = "admin@example.com";
    avatar = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTMnhLfMI8-AZWBEqatrnvoCTZY9_K-awqOYw&s";
  };
  (* Add some default price items *)
  Hashtbl.add prices "1" { id = "1"; name = "Coffee"; price = 3.50; category = "Beverages" };
  Hashtbl.add prices "2" { id = "2"; name = "Croissant"; price = 2.80; category = "Bakery" };
  Hashtbl.add prices "3" { id = "3"; name = "Petit Pain"; price = 0.90; category = "Bakery" };
  Hashtbl.add prices "4" { id = "4"; name = "Tea"; price = 2.50; category = "Beverages" }

let check_credentials username password =
  match Hashtbl.find_opt users username with
  | Some user -> 
    let result = user.password = password in
    Dream.log "Checking credentials: username=%s, password=%s, result=%b" username password result;
    result
  | None -> false

let get_user username =
  Hashtbl.find_opt users username

let get_all_prices () =
  Hashtbl.fold (fun _ item acc -> item :: acc) prices []
  |> List.sort (fun a b -> String.compare a.category b.category)

let get_price_item id =
  Hashtbl.find_opt prices id

let next_price_id =
  let counter = ref 5 in
  fun () ->
    let id = string_of_int !counter in
    incr counter;
    id

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
      background: #f4a460;
      position: relative;
      overflow: hidden;
    }
    body::before {
      content: '';
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      background-image: url('data:image/svg+xml;utf8,<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 200 200"><text x="50%%" y="50%%" font-size="120" text-anchor="middle" dominant-baseline="middle">🐫</text></svg>');
      background-size: cover;
      background-position: center;
      background-repeat: no-repeat;
      opacity: 0.2;
      z-index: 0;
    }
    body::after {
      content: '';
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      background: linear-gradient(135deg, rgba(102, 126, 234, 0.6) 0%%, rgba(118, 75, 162, 0.6) 100%%);
      z-index: 1;
    }
    .login-container {
      background: rgba(255, 255, 255, 0.95);
      padding: 2rem;
      border-radius: 10px;
      box-shadow: 0 8px 32px rgba(0, 0, 0, 0.3);
      width: 300px;
      position: relative;
      z-index: 2;
      backdrop-filter: blur(5px);
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
    <h1>🐫 OCaml Login</h1>
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
      flex-wrap: wrap;
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
      <a href="/prices" class="secondary">Price Management</a>
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

let prices_page success_msg =
  let items = get_all_prices () in
  let items_html = 
    List.map (fun item ->
      Printf.sprintf {|
        <tr>
          <td>%s</td>
          <td>%s</td>
          <td>%.2f €</td>
          <td>
            <a href="/prices/edit/%s" class="btn-edit">Edit</a>
            <form method="POST" action="/prices/delete/%s" style="display:inline;">
              <button type="submit" class="btn-delete" onclick="return confirm('Are you sure?')">Delete</button>
            </form>
          </td>
        </tr>
      |} item.name item.category item.price item.id item.id
    ) items
    |> String.concat "\n"
  in
  Dream.html @@
  Printf.sprintf {|
<!DOCTYPE html>
<html>
<head>
  <title>Price Management</title>
  <style>
    body {
      font-family: Arial, sans-serif;
      min-height: 100vh;
      margin: 0;
      background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%);
      padding: 2rem;
    }
    .container {
      max-width: 900px;
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
    .success {
      color: #38a169;
      text-align: center;
      margin-bottom: 1rem;
      padding: 0.75rem;
      background: #c6f6d5;
      border-radius: 5px;
    }
    .actions {
      display: flex;
      justify-content: space-between;
      margin-bottom: 2rem;
    }
    .btn {
      padding: 0.75rem 1.5rem;
      border-radius: 5px;
      text-decoration: none;
      color: white;
      font-weight: bold;
    }
    .btn-primary {
      background: #667eea;
    }
    .btn-primary:hover {
      background: #5568d3;
    }
    .btn-secondary {
      background: #48bb78;
    }
    .btn-secondary:hover {
      background: #38a169;
    }
    table {
      width: 100%%;
      border-collapse: collapse;
      margin-bottom: 2rem;
    }
    th, td {
      padding: 1rem;
      text-align: left;
      border-bottom: 1px solid #ddd;
    }
    th {
      background: #f7fafc;
      font-weight: bold;
      color: #333;
    }
    .btn-edit, .btn-delete {
      padding: 0.5rem 1rem;
      border-radius: 5px;
      text-decoration: none;
      color: white;
      font-size: 0.9rem;
      border: none;
      cursor: pointer;
      margin-right: 0.5rem;
    }
    .btn-edit {
      background: #4299e1;
    }
    .btn-edit:hover {
      background: #3182ce;
    }
    .btn-delete {
      background: #e53e3e;
    }
    .btn-delete:hover {
      background: #c53030;
    }
  </style>
</head>
<body>
  <div class="container">
    <h1>💰 Price Management</h1>
    %s
    <div class="actions">
      <a href="/prices/new" class="btn btn-primary">+ Add New Item</a>
      <a href="/welcome" class="btn btn-secondary">Back to Dashboard</a>
    </div>
    <table>
      <thead>
        <tr>
          <th>Name</th>
          <th>Category</th>
          <th>Price</th>
          <th>Actions</th>
        </tr>
      </thead>
      <tbody>
        %s
      </tbody>
    </table>
  </div>
</body>
</html>
|}
    (match success_msg with
     | Some msg -> Printf.sprintf {|<div class="success">%s</div>|} msg
     | None -> "")
    items_html

let price_form_page item_opt error_msg =
  let (title, name, price, category, action) = match item_opt with
    | Some item -> ("Edit Price", item.name, item.price, item.category, Printf.sprintf "/prices/update/%s" item.id)
    | None -> ("Add New Price", "", 0.0, "", "/prices/create")
  in
  Dream.html @@
  Printf.sprintf {|
<!DOCTYPE html>
<html>
<head>
  <title>%s</title>
  <style>
    body {
      font-family: Arial, sans-serif;
      min-height: 100vh;
      margin: 0;
      background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%);
      padding: 2rem;
    }
    .container {
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
    .error {
      color: #e53e3e;
      text-align: center;
      margin-bottom: 1rem;
      padding: 0.75rem;
      background: #fed7d7;
      border-radius: 5px;
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
  <div class="container">
    <h1>%s</h1>
    %s
    <form method="POST" action="%s">
      <div class="form-group">
        <label>Name</label>
        <input type="text" name="name" value="%s" required>
      </div>
      <div class="form-group">
        <label>Category</label>
        <input type="text" name="category" value="%s" required>
      </div>
      <div class="form-group">
        <label>Price (€)</label>
        <input type="number" step="0.01" name="price" value="%.2f" required>
      </div>
      <button type="submit">Save</button>
    </form>
    <a href="/prices" class="back-link">← Back to Price List</a>
  </div>
</body>
</html>
|} title title
    (match error_msg with
     | Some msg -> Printf.sprintf {|<div class="error">%s</div>|} msg
     | None -> "")
    action name category price

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
             user.username <- u;
             user.email <- e;
             user.avatar <- a;
             (match password with
              | Some p when String.length p > 0 -> user.password <- p
              | _ -> ());
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

let show_prices request =
  match Dream.session_field request "user" with
  | Some _ ->
    let flash = Dream.flash_messages request in
    let success_msg = 
      match flash with
      | (_, msg) :: _ -> Some msg
      | [] -> None
    in
    prices_page success_msg
  | None -> Dream.redirect request "/"

let show_price_form request id_opt =
  match Dream.session_field request "user" with
  | Some _ ->
    let item = match id_opt with
      | Some id -> get_price_item id
      | None -> None
    in
    price_form_page item None
  | None -> Dream.redirect request "/"

let create_price request =
  match Dream.session_field request "user" with
  | Some _ ->
    (match%lwt Dream.form ~csrf:false request with
     | `Ok fields ->
       let name = List.assoc_opt "name" fields in
       let category = List.assoc_opt "category" fields in
       let price_str = List.assoc_opt "price" fields in
       (match name, category, price_str with
        | Some n, Some c, Some p ->
          (try
            let price = float_of_string p in
            let id = next_price_id () in
            Hashtbl.add prices id { id; name = n; price; category = c };
            Dream.add_flash_message request "Success" "Price added successfully!";
            Dream.redirect request "/prices"
           with _ ->
            Dream.add_flash_message request "Error" "Invalid price value";
            Dream.redirect request "/prices/new")
        | _ ->
          Dream.add_flash_message request "Error" "Missing required fields";
          Dream.redirect request "/prices/new")
     | _ ->
       Dream.add_flash_message request "Error" "Invalid form data";
       Dream.redirect request "/prices/new")
  | None -> Dream.redirect request "/"

let update_price request id =
  match Dream.session_field request "user" with
  | Some _ ->
    (match%lwt Dream.form ~csrf:false request with
     | `Ok fields ->
       let name = List.assoc_opt "name" fields in
       let category = List.assoc_opt "category" fields in
       let price_str = List.assoc_opt "price" fields in
       (match get_price_item id with
        | Some item ->
          (match name, category, price_str with
           | Some n, Some c, Some p ->
             (try
               let price = float_of_string p in
               item.name <- n;
               item.category <- c;
               item.price <- price;
               Dream.add_flash_message request "Success" "Price updated successfully!";
               Dream.redirect request "/prices"
              with _ ->
               Dream.add_flash_message request "Error" "Invalid price value";
               Dream.redirect request (Printf.sprintf "/prices/edit/%s" id))
           | _ ->
             Dream.add_flash_message request "Error" "Missing required fields";
             Dream.redirect request (Printf.sprintf "/prices/edit/%s" id))
        | None ->
          Dream.add_flash_message request "Error" "Price item not found";
          Dream.redirect request "/prices")
     | _ ->
       Dream.add_flash_message request "Error" "Invalid form data";
       Dream.redirect request (Printf.sprintf "/prices/edit/%s" id))
  | None -> Dream.redirect request "/"

let delete_price request id =
  match Dream.session_field request "user" with
  | Some _ ->
    Hashtbl.remove prices id;
    Dream.add_flash_message request "Success" "Price deleted successfully!";
    Dream.redirect request "/prices"
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
    Dream.get  "/"                    show_login_form;
    Dream.post "/login"               handle_login;
    Dream.get  "/welcome"             show_welcome;
    Dream.get  "/profile"             show_profile;
    Dream.post "/profile/update"      update_profile;
    Dream.get  "/prices"              show_prices;
    Dream.get  "/prices/new"          (fun r -> show_price_form r None);
    Dream.post "/prices/create"       create_price;
    Dream.get  "/prices/edit/:id"     (fun r -> show_price_form r (Some (Dream.param r "id")));
    Dream.post "/prices/update/:id"   (fun r -> update_price r (Dream.param r "id"));
    Dream.post "/prices/delete/:id"   (fun r -> delete_price r (Dream.param r "id"));
    Dream.get  "/logout"              handle_logout;
  ]
