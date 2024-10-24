document.addEventListener('DOMContentLoaded', function () {
	AOS.init();
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
});

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
	const molly_csrf = document.getElementById("molly-csrf").value.trim();
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
	}, 1600);
}

async function deployUnikernel() {
	const deployButton = document.getElementById("deploy-button");
	const name = document.getElementById("unikernel-name").value.trim();
	const arguments = document.getElementById("unikernel-arguments").value.trim();
	const binary = document.getElementById("unikernel-binary").files[0];
	const molly_csrf = document.getElementById("molly-csrf").value.trim();
	const formAlert = document.getElementById("form-alert");
	if (!name || !binary) {
		formAlert.classList.remove("hidden", "text-primary-500");
		formAlert.classList.add("text-secondary-500");
		formAlert.textContent = "Please fill in the required data"
		buttonLoading(deployButton, false, "Deploy")
	} else {
		buttonLoading(deployButton, true, "Deploying...")
		let formData = new FormData();
		formData.append("name", name);
		formData.append("binary", binary)
		formData.append("arguments", arguments)
		formData.append("molly_csrf", molly_csrf)
		try {
			const response = await fetch("/unikernel/create", {
				method: 'POST',
				body: formData
			})
			const data = await response.json();
			if (data.status === 200 && data.success) {
				formAlert.classList.remove("hidden", "text-secondary-500");
				formAlert.classList.add("text-primary-500");
				formAlert.textContent = "Succesfully updated";
				postAlert("bg-primary-300", `${name} has been deployed succesfully.`);
				setTimeout(function () {
					window.location.href = "/dashboard";
				}, 2000);
			} else {
				postAlert("bg-secondary-300", data.data);
				formAlert.classList.remove("hidden", "text-primary-500");
				formAlert.classList.add("text-secondary-500");
				formAlert.textContent = data.data
				buttonLoading(deployButton, false, "Deploy")
			}
		} catch (error) {
			postAlert("bg-secondary-300", error);
			formAlert.classList.remove("hidden");
			formAlert.classList.add("text-secondary-500");
			formAlert.textContent = error
			buttonLoading(deployButton, false, "Deploy")
		}
	}
}

async function restartUnikernel(name) {
	try {
		const molly_csrf = document.getElementById("molly-csrf").value.trim();
		const response = await fetch(`/unikernel/restart/${name}`, {
			method: 'POST',
			body: JSON.stringify({ "name": name, "molly_csrf": molly_csrf }),
			headers: { 'Content-Type': 'application/json' }
		})

		const data = await response.json();
		if (data.status === 200) {
			postAlert("bg-primary-300", `Successful: ${data.data}`);
			setTimeout(function () {
				window.location.href = "/dashboard";
			}, 2000);
		} else {
			postAlert("bg-secondary-300", `${name} has been restarted succesfully.`);
		}
	} catch (error) {
		postAlert("bg-secondary-300", error);
	}
}

async function destroyUnikernel(name) {
	try {
		const molly_csrf = document.getElementById("molly-csrf").value.trim();
		const response = await fetch(`/unikernel/destroy/${name}`, {
			method: 'POST',
			body: JSON.stringify({ "name": name, "molly_csrf": molly_csrf }),
			headers: { 'Content-Type': 'application/json' }
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
		btn.classList.remove("bg-primary-500", "text-gray-50");
		btn.classList.add("bg-primary-50", "text-primary-950");
		btn.innerHTML = `<i class="fa-solid fa-spinner animate-spin mr-2"></i> ${text}`;
	} else {
		btn.disabled = false;
		btn.classList.remove("bg-primary-50", "text-primary-950");
		btn.classList.add("bg-primary-500", "text-gray-50");
		btn.innerHTML = text;
	}
}

async function toggleUserStatus(uuid, endpoint) {
	try {
		const molly_csrf = document.getElementById("molly-csrf").value.trim();
		const response = await fetch(endpoint, {
			method: 'POST',
			body: JSON.stringify({ uuid, molly_csrf }),
			headers: { 'Content-Type': 'application/json' }
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
	const molly_csrf = document.getElementById("molly-csrf").value.trim();
	try {
		buttonLoading(policyButton, true, "Processing...")
		const response = await fetch("/api/admin/u/policy/update", {
			method: 'POST',
			headers: {
				"Content-Type": "application/json",
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

