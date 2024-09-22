document.addEventListener('DOMContentLoaded', function () {
	AOS.init();
	if (window.location.pathname.startsWith("/unikernel/info")) {
		setInterval(getConsoleOutput, 300);
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
});

function getUnikernelName(url) {
	const urlObj = new URL(url);
	const pathParts = urlObj.pathname.split('/');
	return pathParts[pathParts.length - 1];
}

function filterData() {
	var input, filter, table, tr, td, i, txtValue;
	input = document.getElementById("searchQuery");
	filter = input.value.toUpperCase();
	table = document.getElementById("data-table");
	tr = table.getElementsByTagName("tr");
	for (i = 0; i < tr.length; i++) {
		td = tr[i].getElementsByTagName("td")[0];
		if (td) {
			txtValue = td.textContent || td.innerText;
			if (txtValue.toUpperCase().indexOf(filter) > -1) {
				tr[i].style.display = "";
			} else {
				tr[i].style.display = "none";
			}
		}
	}
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
	formButton.classList.add("disabled");
	formButton.innerHTML = `<i class="fa-solid fa-spinner animate-spin"></i>`
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
				body: JSON.stringify({ "server_ip": ipInput, "server_port": Number(portInput), "certificate": certificateInput, "private_key": pkeyInput })
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

async function destroyUnikernel(name) {
	try {
		const response = await fetch(`/unikernel/destroy/${name}`, {
			method: 'GET',
			mode: "no-cors"
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



var consoleArea = document.createElement("div");
async function getConsoleOutput() {
	let unikernel_name = getUnikernelName(window.location.href);
	consoleArea.classList.add("w-full", "bg-transparent", "border-0", "h-screen", "overflow-hidden");
	if (unikernel_name) {
		const consoleDiv = document.getElementById("console-container");
		try {
			const response = await fetch("/unikernel/console/" + unikernel_name, {
				method: "GET",
			});
			const responseText = await response.json();

			if (response.status === 200) {
				consoleArea.classList.remove("text-secondary-500");
				consoleArea.innerHTML = "";
				responseText.forEach(log => {
					const timeElement = document.createElement("p");
					timeElement.classList.add("text-gray-600", "font-mono", "text-sm");
					const lineElement = document.createElement("p");
					lineElement.classList.add("text-white", "font-mono", "text-sm");
					lineElement.textContent = log.line;
					consoleArea.appendChild(timeElement);
					consoleArea.appendChild(lineElement);
				});
			} else {
				consoleArea.classList.add("text-secondary-500");
				consoleArea.innerHTML = `Error: ${responseText.data}<br>`;
			}
		} catch (error) {
			consoleArea.classList.add("text-secondary-500");
			consoleArea.innerHTML = `Error: ${unikernel_name} ${error}<br>`;
		}
		consoleDiv.appendChild(consoleArea);
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
		const response = await fetch(endpoint, {
			method: 'POST',
			body: JSON.stringify({ uuid: uuid }),
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
