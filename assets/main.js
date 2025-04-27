document.addEventListener('DOMContentLoaded', function () {
	AOS.init();

	const flashMessage = getCookie('flash_msg');
	if (flashMessage) {
		if (flashMessage.startsWith("error:")) {
			postAlert("bg-secondary-300", flashMessage);
		} else {
			postAlert("bg-primary-300", flashMessage);
		}
		setTimeout(() => {
			document.cookie = "flash_msg=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 UTC;";
		}, 400);
	}

	if (window.location.pathname.startsWith("/admin/user/")) {
		const tabs = document.querySelectorAll(".tab-link");
		const tabPanes = document.querySelectorAll(".tab-pane");

		tabs.forEach((tab) => {
			tab.addEventListener("click", function (event) {
				event.preventDefault();

				// Remove active class from all tabs
				tabs.forEach((t) => t.classList.remove("active"));

				// Hide all tab content
				tabPanes.forEach((pane) => pane.classList.add("hidden"));

				// Add active class to the clicked tab
				tab.classList.add("active");

				// Show the corresponding tab content
				const target = document.querySelector(tab.getAttribute("href"));
				target.classList.remove("hidden");
			});
		});
	}

	if (window.location.pathname.startsWith("/unikernel/update/")) {
		const targetElements = document.querySelectorAll(".code-diff");
		targetElements.forEach((targetElement) => {
			const diffString = targetElement.textContent;
			const configuration = {
				drawFileList: false,
				fileListToggle: false,
				fileListStartVisible: false,
				fileContentToggle: false,
				matching: 'none',
				outputFormat: 'line-by-line',
				synchronisedScroll: true,
				highlight: true,
				renderNothingWhenEmpty: false,
			};
			var diff2htmlUi = new Diff2HtmlUI(targetElement, diffString, configuration);
			diff2htmlUi.draw();
			diff2htmlUi.highlightCode();
		});
	}
});

function getCookie(name) {
	const cookies = document.cookie.split(";");
	for (let cookie of cookies) {
		const [cookieName, cookieValue] = cookie.split("=");
		if (cookieName === name) {
			return decodeURIComponent(cookieValue);
		}
	}
}

function getUnikernelName(url) {
	const urlObj = new URL(url);
	const pathParts = urlObj.pathname.split('/');
	return pathParts[pathParts.length - 1];
}

function filterData() {
	const input = document.getElementById("searchQuery").value.toUpperCase();
	const table = document.getElementById("data-table");
	const rows = Array.from(table.querySelectorAll("tbody tr"));

	rows.forEach(row => {
		const cells = Array.from(row.getElementsByTagName("td"));
		const match = cells.some(td => td.textContent.toUpperCase().includes(input));
		row.style.display = match ? "" : "none";
	});
}



function openConfigForm(ip, port, certificate, p_key) {
	const formSection = document.getElementById("config-form");
	const configSection = document.getElementById("config-body");
	const ipInput = document.getElementById("server-ip");
	const portInput = document.getElementById("server-port");
	const certificateInput = document.getElementById("certificate");
	const pkeyInput = document.getElementById("private-key");
	const configBtn = document.getElementById("config-button");
	const addConfigBtn = document.getElementById("add-config")
	ipInput.value = ip;
	portInput.value = port;
	certificateInput.value = certificate;
	pkeyInput.value = p_key;
	formSection.classList.remove("hidden");
	formSection.classList.add("block");
	configSection.classList.remove("block");
	configSection.classList.add("hidden");
	addConfigBtn.classList.add("hidden");
	if (ip === '' || port === '' || certificate === '' || p_key === '') {
		configBtn.textContent = "Save";
	} else {
		configBtn.textContent = "Update";
	}
}

async function saveConfig() {

	const ipInput = document.getElementById("server-ip").value;
	const portInput = document.getElementById("server-port").value;
	const certificateInput = document.getElementById("certificate").value;
	const pkeyInput = document.getElementById("private-key").value;
	const formAlert = document.getElementById("form-alert");
	const formButton = document.getElementById('config-button');
	const molly_csrf = document.getElementById("molly-csrf").value;
	formButton.classList.add("disabled");
	formButton.innerHTML = `Processing <i class="fa-solid fa-spinner animate-spin text-primary-800"></i>`
	formButton.disabled = true;
	if (ipInput === '' || portInput === '' || certificateInput === '' || pkeyInput === '') {
		formAlert.classList.remove("hidden");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "Please fill all fields";
	} else {
		try {
			const response = await fetch("/api/admin/settings/update", {
				method: 'POST',
				headers: {
					"Content-Type": "application/json",
					"Accept": "application/json",
				},
				body: JSON.stringify({
					"server_ip": ipInput,
					"server_port": Number(portInput),
					"certificate": certificateInput,
					"private_key": pkeyInput,
					"molly_csrf": molly_csrf
				})
			})
			const data = await response.json();
			if (data.status === 200) {
				formAlert.classList.remove("hidden", "text-secondary-500");
				formAlert.classList.add("text-primary-500");
				formAlert.textContent = "Succesfully updated";
				postAlert("bg-primary-300", data.data);
				setTimeout(function () {
					window.location.reload();
				}, 2000);
			} else {
				formAlert.classList.remove("hidden", "text-primary-500");
				formAlert.classList.add("text-secondary-500");
				formAlert.textContent = data.data
			}
		} catch (error) {
			formAlert.classList.remove("hidden");
			formAlert.classList.add("text-secondary-500");
			formAlert.textContent = error
		}
	}
	formButton.innerHTML = "Update"
	formButton.disabled = false;
}

function closeBanner() {
	var banner = document.getElementById("banner-message");
	banner.style.display = "none";
}

function postAlert(bg_color, content) {
	const alertContainer = document.getElementById("alert-container");
	alertContainer.classList.remove("hidden")
	alertContainer.classList.add("block", `${bg_color}`, "text-white", "transition", "ease-in-out", "delay-150", "duration-300")
	const alert = document.createElement("div");
	alert.className = `text-white transition ease-in-out delay-150 duration-300 ${bg_color}`;
	alert.textContent = content;
	alertContainer.appendChild(alert);
	setTimeout(() => {
		alertContainer.classList.remove("block", `${bg_color}`)
		alertContainer.classList.add("hidden")
		alertContainer.removeChild(alert);
	}, 4000);
}

function gatherFieldsForDevices(fieldId, targetKey, formAlert) {
	const result = [];
	let index = 0;
	let loop = true;

	while (loop) {
		const selectEl = document.getElementById(`${fieldId}-select-${index}`);
		const inputEl = document.getElementById(`${fieldId}-input-${index}`);

		// Case: One of them exists but the other doesn't
		if ((selectEl && !inputEl) || (!selectEl && inputEl)) {
			formAlert.classList.remove("hidden", "text-primary-500");
			formAlert.classList.add("text-secondary-500");
			formAlert.textContent = `Please fill in both the ${fieldId} device and its associated name.`;
			postAlert("bg-secondary-300", `Please fill in both the ${fieldId} device and its associated name.`);
			return null;
		}

		// Case: both don't exist => end loop
		if (!selectEl && !inputEl) {
			break;
		}

		// Otherwise, both exist -> proceed
		const selectValue = selectEl.value.trim();
		const inputValue = inputEl.value.trim();

		// check for empty strings:
		if (!selectValue || !inputValue) {
			formAlert.classList.remove("hidden", "text-primary-500");
			formAlert.classList.add("text-secondary-500");
			formAlert.textContent = `Please fill in both the ${fieldId} device and its associated name.`;
			postAlert("bg-secondary-300", `Please fill in both the ${fieldId} device and its associated name.`);
			return null;
		}

		result.push({
			name: inputValue,
			host_device: selectValue,
		});

		index++;
	}

	return { [targetKey]: result };
}

async function deployUnikernel() {
	const formAlert = document.getElementById("form-alert");
	const deployButton = document.getElementById("deploy-button");
	const name = document.getElementById("unikernel-name").value;
	const cpuid = document.getElementById("cpuid").value;
	const fail_behaviour = document.getElementById("restart-on-fail").checked;
	const memory = document.getElementById("unikernel-memory").innerText;

	const networkData = gatherFieldsForDevices("network", "network_interfaces", formAlert);
	const blockData = gatherFieldsForDevices("block", "block_devices", formAlert);

	// Abort immediately if a device field is invalid
	if (networkData === null || blockData === null) {
		buttonLoading(deployButton, false, "Deploy");
		return;
	}

	const argumentsString = document.getElementById("unikernel-arguments").value.trim();
	const argumentsList = argumentsString ? argumentsString.split(/\s+/) : [];
	const binary = document.getElementById("unikernel-binary").files[0];
	const molly_csrf = document.getElementById("molly-csrf").value;

	if (!isValidName(name) || !binary) {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "Please fill in the required data";
		buttonLoading(deployButton, false, "Deploy");
		return;
	}

	buttonLoading(deployButton, true, "Deploying...");

	let formData = new FormData();
	const unikernel_config = {
		cpuid: Number(cpuid),
		fail_behaviour: fail_behaviour ? { "restart": fail_behaviour } : "quit",
		memory: Number(memory),
		...networkData,
		...blockData,
		arguments: argumentsList
	};

	formData.append("unikernel_name", name,);
	formData.append("unikernel_config", JSON.stringify(unikernel_config));
	formData.append("binary", binary);
	formData.append("molly_csrf", molly_csrf);

	try {
		const response = await fetch("/api/unikernel/create", {
			method: 'POST',
			body: formData
		})
		const data = await response.json();
		if (data.status === 200 && data.success) {
			formAlert.classList.remove("hidden", "text-secondary-500");
			formAlert.classList.add("text-primary-500");
			formAlert.textContent = "Successfully updated";
			postAlert("bg-primary-300", `${name} has been deployed successfully.`);
			setTimeout(function () {
				window.location.href = "/dashboard";
			}, 2000);
		} else {
			postAlert("bg-secondary-300", data.data);
			formAlert.classList.remove("hidden", "text-primary-500");
			formAlert.classList.add("text-secondary-500");
			formAlert.textContent = data.data;
			buttonLoading(deployButton, false, "Deploy");
		}
	} catch (error) {
		postAlert("bg-secondary-300", error);
		formAlert.classList.remove("hidden");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = error;
		buttonLoading(deployButton, false, "Deploy");
	}
}

async function restartUnikernel(name) {
	try {
		const molly_csrf = document.getElementById("molly-csrf").value;
		const response = await fetch(`/api/unikernel/restart`, {
			method: 'POST',
			body: JSON.stringify({ "name": name, "molly_csrf": molly_csrf }),
			headers: { 'Content-Type': 'application/json', 'Accept': 'application/json' }
		})

		const data = await response.json();
		if (data.status === 200) {
			postAlert("bg-primary-300", `Successful: ${data.data}`);
			setTimeout(function () {
				window.location.href = "/dashboard";
			}, 2000);
		} else {
			postAlert("bg-secondary-300", `Error. Restarting ${name} returned ${data.data}.`);
		}
	} catch (error) {
		postAlert("bg-secondary-300", error);
	}
}

async function destroyUnikernel(name) {
	try {
		const molly_csrf = document.getElementById("molly-csrf").value;
		const response = await fetch(`/api/unikernel/destroy`, {
			method: 'POST',
			body: JSON.stringify({ "name": name, "molly_csrf": molly_csrf }),
			headers: { 'Content-Type': 'application/json', 'Accept': 'application/json' }
		})

		const data = await response.json();
		if (data.status === 200) {
			postAlert("bg-primary-300", `Successful: ${data.data}`);
			setTimeout(function () {
				window.location.href = "/dashboard";
			}, 2000);
		} else {
			postAlert("bg-secondary-300", `${name} has been destroyed succesfully.`);
		}
	} catch (error) {
		postAlert("bg-secondary-300", error);
	}
}

function buttonLoading(btn, load, text) {
	if (load) {
		btn.disabled = true;
		btn.innerHTML = `<i class="fa-solid fa-spinner animate-spin mr-2"></i> ${text}`;
	} else {
		btn.disabled = false;
		btn.innerHTML = text;
	}
}

async function toggleUserStatus(uuid, endpoint) {
	try {
		const molly_csrf = document.getElementById("molly-csrf").value;
		const response = await fetch(endpoint, {
			method: 'POST',
			body: JSON.stringify({ uuid, molly_csrf }),
			headers: { 'Content-Type': 'application/json', 'Accept': 'application/json' }
		});

		const data = await response.json();
		if (response.status === 200) {
			postAlert("bg-primary-300", data.data);
			setTimeout(() => window.location.reload(), 1000);
		} else {
			postAlert("bg-secondary-300", data.data);
		}
	} catch (error) {
		postAlert("bg-secondary-300", error);
	}
}

async function toggleUserActiveStatus(uuid) {
	await toggleUserStatus(uuid, "/api/admin/user/activate/toggle");
}

async function toggleUserAdminStatus(uuid) {
	await toggleUserStatus(uuid, "/api/admin/user/admin/toggle");
}


function multiselect(selected, options) {
	return {
		isOpen: false,
		selected: selected,
		options: options,
		toggleDropdown() {
			this.isOpen = !this.isOpen;
		},
		updateSelection(event, option) {
			if (event.target.checked) {
				this.selected.push(option);
			} else {
				this.selected = this.selected.filter(item => item !== option);
			}
		},
		removeItem(index) {
			this.selected.splice(index, 1);
		}
	};
}

async function updatePolicy() {
	const unikernel_count = document.getElementById("f_allowed_unikernels").innerText;
	const mem_size = document.getElementById("f_allowed_memory").innerText;
	const storage_size = document.getElementById("f_allowed_storage").innerText;
	const cpuids = document.getElementById("selectedCPUs").value;
	const bridges = document.getElementById("selectedBridges").value;
	const formAlert = document.getElementById("form-alert");
	const user_id = document.getElementById("user_id").innerText;
	const policyButton = document.getElementById("set-policy-btn");
	const molly_csrf = document.getElementById("molly-csrf").value;
	try {
		buttonLoading(policyButton, true, "Processing...")
		const response = await fetch("/api/admin/u/policy/update", {
			method: 'POST',
			headers: {
				"Content-Type": "application/json",
				"Accept": "application/json",
			},
			body: JSON.stringify(
				{
					"unikernels": Number(unikernel_count),
					"memory": Number(mem_size),
					"block": Number(storage_size),
					"cpuids": cpuids,
					"bridges": bridges,
					"user_uuid": user_id,
					"molly_csrf": molly_csrf
				})
		})
		const data = await response.json();
		if (data.status === 200) {
			formAlert.classList.remove("hidden", "text-secondary-500");
			formAlert.classList.add("text-primary-500");
			formAlert.textContent = "Succesfully updated";
			postAlert("bg-primary-300", "Policy updated succesfully");
			setTimeout(function () {
				window.history.back();
			}, 2000);
			buttonLoading(policyButton, false, "Set Policy")
		} else {
			formAlert.classList.remove("hidden", "text-primary-500");
			formAlert.classList.add("text-secondary-500");
			formAlert.textContent = data.data
			buttonLoading(policyButton, false, "Set Policy")
		}
	} catch (error) {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = error
		buttonLoading(policyButton, false, "Set Policy")
	}
}

function sort_data() {
	return {
		sortBy: "",
		sortAsc: false,
		sortByColumn($event) {
			if (this.sortBy === $event.target.innerText) {
				this.sortAsc = !this.sortAsc;
			} else {
				this.sortBy = $event.target.innerText;
				this.sortAsc = true;
			}

			const tableBody = this.getTableBody();
			if (!tableBody) {
				console.error("Table body not found");
				return;
			}

			let rows = this.getTableRows()
				.sort(
					this.sortCallback(
						Array.from($event.target.parentNode.children).indexOf(
							$event.target
						)
					)
				)
				.forEach((tr) => {
					tableBody.appendChild(tr);
				});
		},
		getTableRows() {
			const tableBody = this.getTableBody();
			if (tableBody) {
				return Array.from(tableBody.querySelectorAll("tr"));
			}
			return [];
		},
		getCellValue(row, index) {
			return row.children[index].innerText;
		},
		sortCallback(index) {
			return (a, b) =>
				((row1, row2) => {
					return row1 !== "" &&
						row2 !== "" &&
						!isNaN(row1) &&
						!isNaN(row2)
						? row1 - row2
						: row1.toString().localeCompare(row2);
				})(
					this.getCellValue(this.sortAsc ? a : b, index),
					this.getCellValue(this.sortAsc ? b : a, index)
				);
		},
		getTableBody() {
			return document.querySelector("#data-table tbody");
		}
	};
}

async function updatePassword() {
	const passwordButton = document.getElementById("password-button");
	try {
		buttonLoading(passwordButton, true, "Updating..")
		const molly_csrf = document.getElementById("molly-csrf").value;
		const current_password = document.getElementById("current-password").value;
		const new_password = document.getElementById("new-password").value;
		const confirm_password = document.getElementById("confirm-password").value;
		const formAlert = document.getElementById("form-alert");
		if (!current_password || !new_password || !confirm_password) {
			formAlert.classList.remove("hidden", "text-primary-500");
			formAlert.classList.add("text-secondary-500");
			formAlert.textContent = "Please fill in all the required passwords"
			buttonLoading(passwordButton, false, "Deploy")
		} else {
			const response = await fetch('/account/password/update', {
				method: 'POST',
				body: JSON.stringify(
					{
						molly_csrf,
						current_password,
						new_password,
						confirm_password

					}),
				headers: { 'Content-Type': 'application/json', 'Accept': 'application/json' }
			});

			const data = await response.json();
			if (response.status === 200) {
				postAlert("bg-primary-300", data.data);
				setTimeout(() => window.location.reload(), 1000);
			} else {
				postAlert("bg-secondary-300", data.data);
				buttonLoading(passwordButton, false, "Save")
			}
		}
	} catch (error) {
		postAlert("bg-secondary-300", error);
		buttonLoading(passwordButton, false, "Save")
	}
}

async function closeSessions() {
	const sessionButton = document.getElementById("session-button");
	try {
		buttonLoading(sessionButton, true, "Closing sessions..")
		const molly_csrf = document.getElementById("molly-csrf").value;
		const response = await fetch('/api/account/sessions/close', {
			method: 'POST',
			body: JSON.stringify(
				{
					molly_csrf,
				}),
			headers: { 'Content-Type': 'application/json', 'Accept': 'application/json' }
		});

		const data = await response.json();
		if (response.status === 200) {
			postAlert("bg-primary-300", data.data);
			setTimeout(() => window.location.reload(), 1000);
		} else {
			postAlert("bg-secondary-300", data.data);
			buttonLoading(sessionButton, false, "Logout all other sessions")
		}
	} catch (error) {
		postAlert("bg-secondary-300", error);
		buttonLoading(sessionButton, false, "Logout all other sessions")
	}
}

async function closeSession(session_value) {
	const sessionButton = document.getElementById(`session-button-${session_value}`);
	try {
		buttonLoading(sessionButton, true, "Closing session..")
		const molly_csrf = document.getElementById("molly-csrf").value;
		const response = await fetch('/api/account/session/close', {
			method: 'POST',
			body: JSON.stringify(
				{
					session_value,
					molly_csrf,
				}),
			headers: { 'Content-Type': 'application/json', 'Accept': 'application/json' }
		});

		const data = await response.json();
		if (response.status === 200) {
			postAlert("bg-primary-300", data.data);
			setTimeout(() => window.location.reload(), 1000);
		} else {
			postAlert("bg-secondary-300", data.data);
			buttonLoading(sessionButton, false, "Close session")
		}
	} catch (error) {
		postAlert("bg-secondary-300", error);
		buttonLoading(sessionButton, false, "Close session")
	}
}

async function logout() {
	const logoutButton = document.getElementById("logout-button");
	try {
		buttonLoading(logoutButton, true, "Closing session..")
		const molly_csrf = document.getElementById("molly-csrf").value;
		fetch('/logout', {
			method: 'POST',
			body: JSON.stringify(
				{
					molly_csrf,
				}),
			headers: { 'Content-Type': 'application/json' }
		});
		setTimeout(() => window.location.reload(), 1000);
	} catch (error) {
		postAlert("bg-secondary-300", error);
		buttonLoading(logoutButton, false, "Logout")
	}
}

async function deleteVolume(block_name) {
	const deleteButton = document.getElementById(`delete-block-button-${block_name}`);
	const molly_csrf = document.getElementById("molly-csrf").value;
	const formAlert = document.getElementById("form-alert");
	try {
		buttonLoading(deleteButton, true, "Deleting...")
		const response = await fetch("/api/volume/delete", {
			method: 'POST',
			headers: {
				"Content-Type": "application/json",
				"Accept": "application/json",
			},
			body: JSON.stringify(
				{
					"block_name": block_name,
					"molly_csrf": molly_csrf
				})
		})
		const data = await response.json();
		if (data.status === 200) {
			formAlert.classList.remove("hidden", "text-secondary-500");
			formAlert.classList.add("text-primary-500");
			formAlert.textContent = "Successfully deleted";
			postAlert("bg-primary-300", "Volume deleted succesfully");
			setTimeout(() => window.location.reload(), 1000);
			buttonLoading(deleteButton, false, "Delete")
		} else {
			formAlert.classList.remove("hidden", "text-primary-500");
			formAlert.classList.add("text-secondary-500");
			formAlert.textContent = data.data
			buttonLoading(deleteButton, false, "Delete")
		}
	} catch (error) {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = error
		buttonLoading(deleteButton, false, "Delete")
	}
}

async function createVolume() {
	const createButton = document.getElementById("create-block-button");
	const block_name = document.getElementById("block_name").value;
	const block_size = document.getElementById("block_size").innerText;
	const data_toggle = document.getElementById("dataToggle").checked;
	const molly_csrf = document.getElementById("molly-csrf").value;
	const formAlert = document.getElementById("create-volume-form-alert");
	const block_compressed = document.getElementById("block_compressed").checked;
	const block_data = document.getElementById("block_data").files[0];

	if (!block_name || block_name === '') {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "Please enter a name"
		buttonLoading(createButton, false, "Create volume")
		return;
	}
	if (!isLengthValid(block_name)) {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "The name must have at least 1 character and must not exceed 63 characters."
		buttonLoading(createButton, false, "Create volume")
		return;
	}
	if (!isStartingCharacterValid(block_name)) {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "The name cannot start with a hyphen (-)."
		buttonLoading(createButton, false, "Create volume")
		return;
	}
	if (!areCharactersValid(block_name)) {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "Only letters (a-z, A-Z), digits (0-9), hyphens (-), and periods (.) are permitted.\
		 Special characters, spaces, and symbols other than the specified ones are not allowed"
		buttonLoading(createButton, false, "Create volume")
		return;
	}
	if (Number(block_size) < 1) {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "Volume size must be 1MB or greater."
		buttonLoading(createButton, false, "Create volume")
		return;
	}
	if (data_toggle && !block_data) {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "You must upload a file else switch 'Dumb data to this volume' off"
		buttonLoading(createButton, false, "Create volume")
		return;
	}

	try {
		buttonLoading(createButton, true, "Creating...")
		let formData = new FormData();
		let json_data = JSON.stringify(
			{
				"block_name": block_name,
				"block_size": Number(block_size),
				"block_compressed": block_compressed
			})
		formData.append("block_data", block_data)
		formData.append("molly_csrf", molly_csrf)
		formData.append("json_data", json_data)
		const response = await fetch("/api/volume/create", {
			method: 'POST',
			body: formData
		})
		const data = await response.json();
		if (data.status === 200) {
			formAlert.classList.remove("hidden", "text-secondary-500");
			formAlert.classList.add("text-primary-500");
			formAlert.textContent = "Succesfully created";
			postAlert("bg-primary-300", "Volume created succesfully");
			setTimeout(() => window.location.reload(), 1000);
			buttonLoading(createButton, false, "Create volume")
		} else {
			formAlert.classList.remove("hidden", "text-primary-500");
			formAlert.classList.add("text-secondary-500");
			formAlert.textContent = data.data
			buttonLoading(createButton, false, "Create volume")
		}
	} catch (error) {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = error
		buttonLoading(createButton, false, "Create volume")
	}
}

async function downloadVolume(block_name) {
	const downloadButton = document.getElementById(`download-block-button-${block_name}`);
	const compression_level = document.getElementById("compression-level").innerText;
	const molly_csrf = document.getElementById("molly-csrf").value;
	try {
		buttonLoading(downloadButton, true, "Downloading...")
		const response = await fetch("/api/volume/download", {
			method: 'POST',
			headers: {
				"Content-Type": "application/json",
				"Accept": "application/octet-stream,application/json"
			},
			body: JSON.stringify(
				{
					"block_name": block_name,
					"compression_level": Number(compression_level),
					"molly_csrf": molly_csrf
				})
		})
		if (!response.ok) {
			const data = await response.json();
			postAlert("bg-secondary-300", data.data);
			buttonLoading(downloadButton, false, `Download ${block_name}`)
		} else {
			// trigger a download
			const filename = response.headers.get('content-disposition')
				.split(';')[1].split('=')[1].replace(/"/g, '');
			const blob = await response.blob();
			const downloadLink = document.createElement('a');
			downloadLink.href = URL.createObjectURL(blob);
			downloadLink.download = filename;
			downloadLink.click();
			URL.revokeObjectURL(downloadLink.href);
			postAlert("bg-primary-300", "Download in progress");
			buttonLoading(downloadButton, false, `Download ${block_name}`)
		}
	} catch (error) {
		postAlert("bg-secondary-300", error);
		buttonLoading(downloadButton, false, `Download ${block_name}`)
	}
}

async function uploadToVolume(block_name) {
	const uploadButton = document.getElementById(`upload-block-button-${block_name}`);
	const block_compressed = document.getElementById("block_compressed").checked;
	const block_data = document.getElementById("block_data").files[0];
	const molly_csrf = document.getElementById("molly-csrf").value;

	if (!block_data) {
		postAlert("bg-secondary-300", "Please select a file to be uploaded");
		buttonLoading(uploadButton, false, "Upload data")
		return;
	}

	try {
		buttonLoading(uploadButton, true, "Uploading...")
		let formData = new FormData();
		let json_data = JSON.stringify(
			{
				"block_name": block_name,
				"block_compressed": block_compressed
			})
		formData.append("block_data", block_data)
		formData.append("json_data", json_data)
		formData.append("molly_csrf", molly_csrf)
		const response = await fetch("/api/volume/upload", {
			method: 'POST',
			body: formData
		})
		const data = await response.json();
		if (data.status === 200) {
			postAlert("bg-primary-300", `Upload is succesful: ${data.data}`);
			setTimeout(() => window.location.reload(), 1000);
			buttonLoading(uploadButton, false, "Upload data")
		} else {
			postAlert("bg-secondary-300", data.data);
			buttonLoading(uploadButton, false, "Upload data")
		}
	} catch (error) {
		postAlert("bg-secondary-300", error);
		buttonLoading(uploadButton, false, "Upload data")
	}
}


async function createToken() {
	const tokenButton = document.getElementById('create-token-button');
	const token_name = document.getElementById("token_name").value;
	const token_expiry = document.getElementById("token_expiry").value;
	const molly_csrf = document.getElementById("molly-csrf").value;
	const formAlert = document.getElementById("tokens-form-alert");

	if (!token_name || token_name == "") {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "Please enter a name"
		buttonLoading(tokenButton, false, "Create Token")
		return;
	}
	if (!token_expiry || token_expiry == "") {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "Please select a validity period"
		buttonLoading(tokenButton, false, "Create Token")
		return;
	}
	try {
		buttonLoading(tokenButton, true, "Creating...")
		const response = await fetch("/api/tokens/create", {
			method: 'POST',
			headers: {
				"Content-Type": "application/json",
				"Accept": "application/json",
			},
			body: JSON.stringify(
				{
					"token_name": token_name,
					"token_expiry": Number(token_expiry),
					"molly_csrf": molly_csrf
				})
		})
		const data = await response.json();
		if (data.status === 200) {
			postAlert("bg-primary-300", "Token created succesfully");
			setTimeout(() => window.location.reload(), 1000);
			buttonLoading(tokenButton, false, "Create Token")
		} else {
			postAlert("bg-secondary-300", data.data);
			buttonLoading(tokenButton, false, "Create Token")
		}
	} catch (error) {
		postAlert("bg-secondary-300", error);
		buttonLoading(tokenButton, false, "Create Token")
	}
}

async function deleteToken(value) {
	const tokenButton = document.getElementById(`delete-token-button-${value}`);
	const molly_csrf = document.getElementById("molly-csrf").value;
	try {
		buttonLoading(tokenButton, true, "Deleting...")
		const response = await fetch("/api/tokens/delete", {
			method: 'POST',
			headers: {
				"Content-Type": "application/json",
				"Accept": "application/json",
			},
			body: JSON.stringify(
				{
					"token_value": value,
					"molly_csrf": molly_csrf
				})
		})
		const data = await response.json();
		if (data.status === 200) {
			postAlert("bg-primary-300", "Token deleted succesfully");
			setTimeout(() => window.location.reload(), 1000);
			buttonLoading(tokenButton, false, "Delete")
		} else {
			postAlert("bg-secondary-300", data.data);
			buttonLoading(tokenButton, false, "Delete")
		}
	} catch (error) {
		postAlert("bg-secondary-300", error);
		buttonLoading(tokenButton, false, "Delete")
	}
}

async function updateToken(value) {
	const tokenButton = document.getElementById("update-token-button");
	const token_name = document.getElementById("token_u_name").value;
	const token_expiry = document.getElementById("token_u_expiry").value;
	const molly_csrf = document.getElementById("molly-csrf").value;
	const formAlert = document.getElementById("tokens-update-form-alert");

	if (!token_name || token_name == "") {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "Please enter a name"
		buttonLoading(tokenButton, false, "Update Token")
		return;
	}
	if (!token_expiry || token_expiry == "") {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "Please select a validity period"
		buttonLoading(tokenButton, false, "Update Token")
		return;
	}
	try {
		buttonLoading(tokenButton, true, "Updating...")
		const response = await fetch("/api/tokens/update", {
			method: 'POST',
			headers: {
				"Content-Type": "application/json",
				"Accept": "application/json",
			},
			body: JSON.stringify(
				{
					"token_value": value,
					"token_name": token_name,
					"token_expiry": Number(token_expiry),
					"molly_csrf": molly_csrf
				})
		})
		const data = await response.json();
		if (data.status === 200) {
			postAlert("bg-primary-300", "Token updated succesfully");
			setTimeout(() => window.location.reload(), 1000);
			buttonLoading(tokenButton, false, "Update Token")
		} else {
			postAlert("bg-secondary-300", data.data);
			buttonLoading(tokenButton, false, "Update Token")
		}
	} catch (error) {
		postAlert("bg-secondary-300", error);
		buttonLoading(tokenButton, false, "Update Token")
	}
}

async function updateUnikernel(job, to_be_updated_unikernel, currently_running_unikernel, unikernel_name) {
	const updateButton = document.getElementById("update-unikernel-button");
	const unikernelArguments = document.getElementById("unikernel-arguments").value;
	const argumentsToggle = document.getElementById("arguments-toggle").checked;
	const molly_csrf = document.getElementById("molly-csrf").value;
	const formAlert = document.getElementById("unikernel-arguments-alert");
	const liveliness_toggle = document.getElementById("liveliness-toggle").checked;
	const http_toggle = document.getElementById("http-toggle").checked;
	const dns_toggle = document.getElementById("dns-toggle").checked;
	const dns_address = document.getElementById("dns-address").value.trim();
	const dns_name = document.getElementById("dns-name").value.trim();
	const http_address = document.getElementById("http-address").value.trim();

	if (argumentsToggle && !unikernelArguments) {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "You must give arguments for this build else switch 'Update the configuration for this build' off"
		buttonLoading(updateButton, false, "Proceed to update")
		return;
	}

	if (liveliness_toggle && !(http_toggle || dns_toggle)) {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "Error: If liveliness is enabled, at least one of HTTP or DNS must be activated."
		buttonLoading(updateButton, false, "Proceed to update")
		return;
	}

	if (dns_toggle && !(dns_address && dns_name)) {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "Error: DNS is enabled, but DNS name and DNS address is not provided."
		buttonLoading(updateButton, false, "Proceed to update")
		return;
	}

	if (http_toggle && !http_address) {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "Error: HTTP is enabled, but no HTTP address is provided."
		buttonLoading(updateButton, false, "Proceed to update")
		return;
	}

	try {
		buttonLoading(updateButton, true, "Updating...")
		const response = await fetch("/api/unikernel/update", {
			method: 'POST',
			headers: {
				"Content-Type": "application/json",
				"Accept": "application/json",
			},
			body: JSON.stringify(
				{
					"job": job,
					"to_be_updated_unikernel": to_be_updated_unikernel,
					"currently_running_unikernel": currently_running_unikernel,
					"unikernel_name": unikernel_name,
					"unikernel_arguments": argumentsToggle ? JSON.parse(unikernelArguments) : null,
					"http_liveliness_address": http_toggle ? (http_address ? http_address : null) : null,
					"dns_liveliness": dns_toggle ? ({
						"dns_address": dns_address ? dns_address : null,
						"dns_name": dns_name ? dns_name : null
					}) : null,
					"molly_csrf": molly_csrf
				})
		})
		const data = await response.json();
		if (data.status === 200) {
			postAlert("bg-primary-300", "Unikernel updated succesfully");
			setTimeout(() => window.location.reload(), 1000);
			buttonLoading(updateButton, false, "Proceed to update")
		} else {
			postAlert("bg-secondary-300", data.data);
			formAlert.classList.remove("hidden", "text-primary-500");
			formAlert.classList.add("text-secondary-500");
			formAlert.textContent = data.data
			buttonLoading(updateButton, false, "Proceed to update")
		}
	} catch (error) {
		postAlert("bg-secondary-300", error);
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = data.data
		buttonLoading(updateButton, false, "Proceed to update")
	}
}

async function rollbackUnikernel(unikernel_name) {
	const rollbackButton = document.getElementById("unikernel-rollback");
	const molly_csrf = document.getElementById("molly-csrf").value;
	const formAlert = document.getElementById("form-alert");
	try {
		buttonLoading(rollbackButton, true, "Rolling back...")
		const response = await fetch("/api/unikernel/rollback", {
			method: 'POST',
			headers: {
				"Content-Type": "application/json",
				"Accept": "application/json",
			},
			body: JSON.stringify(
				{
					"unikernel_name": unikernel_name,
					"molly_csrf": molly_csrf
				})
		})
		const data = await response.json();
		if (data.status === 200) {
			postAlert("bg-primary-300", "Unikernel rollback succesful");
			setTimeout(() => window.location.reload(), 1000);
			buttonLoading(rollbackButton, false, "Rollback")
		} else {
			postAlert("bg-secondary-300", data.data);
			formAlert.classList.remove("hidden", "text-primary-500");
			formAlert.classList.add("text-secondary-500");
			formAlert.textContent = data.data
			buttonLoading(rollbackButton, false, "Rollback")
		}
	} catch (error) {
		postAlert("bg-secondary-300", error);
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = data.data
		buttonLoading(rollbackButton, false, "Rollback")
	}
}

function isValidName(s) {
	const length = s.length;
	if (length === 0 || length >= 64) return false;
	if (s[0] === '-') return false;
	for (let i = 0; i < length; i++) {
		const char = s[i];
		if (!(/[a-zA-Z0-9.-]/).test(char)) {
			return false;
		}
	}
	return true;
}

function isLengthValid(s) {
	const length = s.length;
	return length > 0 && length < 64;
}

function isStartingCharacterValid(s) {
	return s[0] !== '-';
}

function areCharactersValid(s) {
	for (let i = 0; i < s.length; i++) {
		const char = s[i];
		if (!(/[a-zA-Z0-9.-]/).test(char)) {
			return false;
		}
	}
	return true;
}
