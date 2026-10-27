let consoleLogEvent = null;

const consoleLogEndpoints = ["/unikernel/info", "/unikernel/console"];

document.addEventListener('DOMContentLoaded', function () {
	AOS.init();

	const currentPath = window.location.pathname;
	const sidebarItems = document.querySelectorAll('.w-full.my-6 > a');
	sidebarItems.forEach(a => {
		if (a.getAttribute('href') === currentPath) {
			a.classList.add('bg-primary-100', 'text-primary-600', 'font-bold');
			a.classList.remove('hover:bg-gray-200', 'hover:text-primary-400');
		}
	});

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

	if (consoleLogEndpoints.includes(window.location.pathname)) {
		startEventSource();
	}
});

function startEventSource() {
	if (consoleLogEvent) return;

	if (!consoleLogEndpoints.includes(window.location.pathname)) return;

	const params = new URLSearchParams(window.location.search);
	let unikernelName = params.get("unikernel");
	let albatrossInstance = params.get("instance");
	if (!unikernelName || !albatrossInstance) {
		console.error("No albatross instance selected and no unikernel provided.");
	}
	const console_output = document.getElementById("console-output");

	const MAX_LOG_ENTRIES = 40;
	let logBuffer = [];

	const render = () => {
		console_output.value = logBuffer.join("\n");
	};

	consoleLogEvent = new EventSource(`/api/unikernel/console?unikernel=${unikernelName}&instance=${albatrossInstance}`);

	consoleLogEvent.onmessage = ({ data }) => {
		try {
			const payload = JSON.parse(data);

			logBuffer = [
				`[${payload.timestamp}] ${payload.line}`,
				...logBuffer.reverse(),
			].slice(0, MAX_LOG_ENTRIES);
			logBuffer.reverse()
			render();
		} catch (err) {
			console.error("Failed to parse SSE payload:", err, data);
		}
	};

}

function stopEventSource() {
	if (consoleLogEvent) {
		consoleLogEvent.close();
		consoleLogEvent = null;
	}
}

document.addEventListener("visibilitychange", () => {
	if (document.visibilityState === "visible") {
		console.log("Resuming SSE stream...");
		startEventSource();
	} else {
		console.log("Pausing SSE stream...");
		stopEventSource();
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

function filterData() {
	const input = document.getElementById("searchQuery").value.toUpperCase();
	const tables = document.querySelectorAll(".data-table");
	tables.forEach(table => {
		const rows = Array.from(table.querySelectorAll("tbody tr"));
		rows.forEach(row => {
			const cells = Array.from(row.getElementsByTagName("td"));
			const match = cells.some(td => td.textContent.toUpperCase().includes(input));
			row.style.display = match ? "" : "none";
		});
	});
}

function filterAlbatrossInstance(event) {
	const searchTerm = event.target.value.toLowerCase();
	const tables = document.querySelectorAll(".data-table");
	tables.forEach(table => {
		const rows = table.querySelectorAll("tbody tr");
		if (searchTerm === "all") {
			rows.forEach(row => {
				row.style.display = "";
			});
		} else {
			rows.forEach(row => {
				const cells = row.querySelectorAll("td");
				const match = Array.from(cells).some(cell =>
					cell.textContent.toLowerCase().includes(searchTerm)
				);
				row.style.display = match ? "" : "none";
			});
		}
	});
}

function openConfigForm(name, ip, port, certificate, p_key) {
	const formSection = document.getElementById("config-form");
	const configSection = document.getElementById("config-body");
	const nameInput = document.getElementById("name");
	const ipInput = document.getElementById("server_ip");
	const portInput = document.getElementById("server_port");
	const certificateInput = document.getElementById("certificate");
	const pkeyInput = document.getElementById("private_key");
	const configBtn = document.getElementById("config-button");
	const addConfigBtn = document.getElementById("add-config");
	nameInput.value = name;
	ipInput.value = ip;
	portInput.value = port;
	certificateInput.value = certificate;
	pkeyInput.value = p_key;
	formSection.classList.remove("hidden");
	formSection.classList.add("block");
	configSection.classList.remove("block");
	configSection.classList.add("hidden");
	addConfigBtn.classList.add("hidden");
	const isNew = name === '' || ip === '' || port === '' || certificate === '' || p_key === '';
	formSection.setAttribute(
		"hx-post",
		isNew ? "/api/admin/settings/albatross/create" : "/api/admin/settings/albatross/update"
	);
	if (window.htmx) {
		htmx.process(formSection);
	}
	configBtn.textContent = isNew ? "Save" : "Update";
}

function closeBanner() {
	var banner = document.getElementById("banner-message");
	banner.style.display = "none";
}

function showError(formAlertElement, msg, isSuccess = false) {
	formAlertElement.classList.remove("hidden", "text-primary-500", "text-secondary-500");
	formAlertElement.classList.add(isSuccess ? "text-primary-500" : "text-secondary-500");
	formAlertElement.textContent = msg;
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
			showError(formAlert, `Please fill in both the ${fieldId} device and its associated name.`);
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
			showError(formAlert, `Please fill in both the ${fieldId} device and its associated name.`);
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

async function deployUnikernel(albatross_instance) {
	const formAlert = document.getElementById("form-alert");
	const deployButton = document.getElementById("deploy-button");
	const handleFailure = (msg) => {
		postAlert("bg-secondary-300", msg);
		showError(formAlert, msg);
		buttonLoading(deployButton, false, "Deploy");
	};
	try {
		const name = document.getElementById("unikernel-name").value;
		const typ = document.getElementById("unikernel-type").value;
		const startup = document.getElementById("startup-priority").innerText;
		const fail_behaviour = document.getElementById("restart-on-fail").checked;
		const force_create = document.getElementById("force-create").checked;
		const memory = document.getElementById("unikernel-memory").innerText;
		const molly_csrf = document.getElementById("molly-csrf").value;
		const cpuidsStr = document.getElementById("cpuids").value;
		const deployModeNode = document.querySelector('input[name="deploy_mode"]:checked');
		const deploy_mode = deployModeNode ? deployModeNode.value : 'manual';
		const binaryInput = document.getElementById("unikernel-binary");
		const binary = (binaryInput && binaryInput.files) ? binaryInput.files[0] : null;
		if (!isValidName(name, formAlert, "unikernel-name")) {
			buttonLoading(deployButton, false, "Deploy");
			return;
		}

		const networkData = gatherFieldsForDevices("network", "network_interfaces", formAlert);
		const blockData = gatherFieldsForDevices("block", "block_devices", formAlert);
		if (!networkData || !blockData) {
			return;
		}
		if (deploy_mode === 'manual' && !binary) {
			showError(formAlert, "Please upload the unikernel image");
			return;
		}

		buttonLoading(deployButton, true, "Deploying...");

		let specificConfig = {};

		if (typ === 'solo5') {
			const argumentsString = document.getElementById("unikernel-arguments").value.trim();
			specificConfig = {
				arguments: argumentsString ? argumentsString.split(/\s+/) : []
			};
		} else if (typ === 'bhyve') {
			const vcpus = document.getElementById("numcpus").innerText;
			const bootPartition = document.getElementById("linux-boot-partition").value;
			specificConfig = {
				vcpus: Number(vcpus),
				linux_boot_partition: bootPartition === "" ? null : bootPartition
			};
		}

		const unikernel_config = {
			typ: typ,
			cpuids: cpuidsStr ? cpuidsStr.split(',').map(Number) : [],
			startup: Number(startup),
			fail_behaviour: fail_behaviour ? { "restart": fail_behaviour } : "quit",
			memory: Number(memory),
			...networkData,
			...blockData,
			...specificConfig
		};

		const formData = new FormData();
		formData.append("albatross_instance", albatross_instance);
		formData.append("unikernel_name", name);
		formData.append("unikernel_config", JSON.stringify(unikernel_config));
		formData.append("unikernel_force_create", force_create);
		formData.append("molly_csrf", molly_csrf);
		formData.append("deploy_mode", deploy_mode);

		if (deploy_mode === 'builder') {
			const builderJobSelect = document.getElementById("builder-job-select");
			if (builderJobSelect) formData.append("builder_job", builderJobSelect.value);
			formData.append("binary", "");
		} else {
			formData.append("binary", binary);
		}

		const response = await fetch("/api/unikernel/create", {
			method: 'POST',
			body: formData
		});
		const data = await response.json();

		if (data.status === 200 && data.success) {
			showError(formAlert, "Successfully updated", true);
			postAlert("bg-primary-300", `${name} has been deployed successfully.`);
			setTimeout(() => {
				window.location.href = "/dashboard";
			}, 2000);
		} else {
			handleFailure(data.data || "An error occurred during deployment.");
		}
	} catch (error) {
		handleFailure(error.message || "A network error occurred.");
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

function sort_data() {
	return {
		sortBy: "",
		sortAsc: false,
		sortByColumn($event) {
			const th = $event.target.closest('th');
			const headerText = th.querySelector('span').textContent.trim();
			if (this.sortBy === headerText) {
				this.sortAsc = !this.sortAsc;
			} else {
				this.sortBy = headerText;
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
						Array.from(th.parentNode.children).indexOf(th)
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

async function createBlock(albatross_instance) {
	const createButton = document.getElementById("create-block-button");
	const block_name = document.getElementById("block_name").value;
	const block_size = document.getElementById("block_size").innerText;
	const data_toggle = document.getElementById("dataToggle").checked;
	const molly_csrf = document.getElementById("molly-csrf").value;
	const formAlert = document.getElementById("create-block-form-alert");
	const block_compressed = document.getElementById("block_compressed").checked;
	const block_data = document.getElementById("block_data").files[0];

	if (!isValidName(block_name, formAlert)) {
		buttonLoading(createButton, false, "Create Block device");
		return;
	}
	if (Number(block_size) < 1) {
		showError(formAlert, "Block device size must be 1MB or greater.");
		buttonLoading(createButton, false, "Create Block device")
		return;
	}
	if (data_toggle && !block_data) {
		showError(formAlert, "You must upload a file else switch 'Dump data to this Block device' off");
		buttonLoading(createButton, false, "Create Block device")
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
		formData.append("molly_csrf", molly_csrf)
		formData.append("albatross_instance", albatross_instance)
		formData.append("json_data", json_data)
		formData.append("block_data", block_data)
		const response = await fetch("/api/block/create", {
			method: 'POST',
			body: formData
		})
		const data = await response.json();
		if (data.status === 200) {
			showError(formAlert, "Successfully created", true);
			postAlert("bg-primary-300", "Block device created successfully");
			setTimeout(() => window.location.reload(), 1000);
			buttonLoading(createButton, false, "Create Block device")
		} else {
			showError(formAlert, data.data);
			buttonLoading(createButton, false, "Create Block device")
		}
	} catch (error) {
		showError(formAlert, error);
		buttonLoading(createButton, false, "Create Block device")
	}
}

async function downloadBlock(block_name, albatross_instance) {
	const downloadButton = document.getElementById(`download-block-button-${block_name}`);
	const compression_level = document.getElementById(`compression-level-${block_name}`).innerText;
	const molly_csrf = document.getElementById("molly-csrf").value;
	try {
		buttonLoading(downloadButton, true, "Downloading...")
		const response = await fetch("/api/block/download", {
			method: 'POST',
			headers: {
				"Content-Type": "application/json",
				"Accept": "application/octet-stream,application/json"
			},
			body: JSON.stringify(
				{
					"block_name": block_name,
					"albatross_instance": albatross_instance,
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

async function uploadToBlock(block_name, albatross_instance) {
	const uploadButton = document.getElementById(`upload-block-button-${block_name}`);
	const block_compressed = document.getElementById(`block_compressed-${block_name}`).checked;
	const block_data = document.getElementById(`block_data_upload-${block_name}`).files[0];
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
		formData.append("json_data", json_data)
		formData.append("albatross_instance", albatross_instance)
		formData.append("molly_csrf", molly_csrf)
		formData.append("block_data", block_data)
		const response = await fetch("/api/block/upload", {
			method: 'POST',
			body: formData
		})
		const data = await response.json();
		if (data.status === 200) {
			postAlert("bg-primary-300", `Upload is successful: ${data.data}`);
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

function isValidName(s, formAlert = null, inputElement = null) {
	let error = null;
	if (!s || s === '') {
		error = "Please enter a name";
	} else if (s.length >= 64) {
		error = "The name must have at least 1 character and must not exceed 63 characters.";
	} else if (s[0] === '-') {
		error = "The name cannot start with a hyphen (-).";
	} else {
		for (let i = 0; i < s.length; i++) {
			if (!(/[a-zA-Z0-9.-]/).test(s[i])) {
				error = "Only letters (a-z, A-Z), digits (0-9), hyphens (-), and periods (.) are permitted. Special characters, spaces, and symbols other than the specified ones are not allowed.";
				break;
			}
		}
	}

	if (error) {
		const inputEl = typeof inputElement === 'string' ? document.getElementById(inputElement) : inputElement;
		if (inputEl) {
			inputEl.classList.add("border-secondary-500", "ring-secondary-500");
		}
		if (formAlert) {
			showError(formAlert, error);
		}
		return false;
	}
	return true;
}

window.mapFields = function (field_id, detail, options) {
	if (!detail || !detail[field_id]) return [];
	let reqs = detail[field_id];
	if (field_id === 'network') {
		let unassigned = [...options];
		let fields = reqs.map(n => {
			let idx = unassigned.findIndex(o => o.value === n);
			if (idx !== -1) {
				let match = unassigned.splice(idx, 1)[0];
				return { title: n, selected: match.value };
			}
			return { title: n, selected: '' };
		});
		if (reqs.length === 1 && options.length === 1 && fields[0].selected === '') {
			fields[0].selected = options[0].value;
		} else if (reqs.length === options.length && unassigned.length === 1) {
			let unmatched = fields.find(f => f.selected === '');
			if (unmatched) unmatched.selected = unassigned[0].value;
		}
		return fields;
	} else if (field_id === 'block') {
		let job_name = detail.job_name;
		return reqs.map(n => {
			let exact = options.find(o => o.value === n);
			if (exact) return { title: n, selected: exact.value };
			if (job_name) {
				let job_match = options.find(o => o.value === job_name || o.value === job_name + '-data');
				if (job_match) return { title: n, selected: job_match.value };
			}
			return { title: n, selected: '' };
		});
	}
	return reqs.map(n => ({ title: n, selected: '' }));
};

function parseXhrResponse(xhr, redirectUrl = null) {
	const formAlert = document.getElementById("form-alert");
	try {
		const res = JSON.parse(xhr.responseText);
		const msg = typeof res.data === "string" ? res.data : res.title;
		if (xhr.status >= 200 && xhr.status < 300) {
			if (formAlert) {
				showError(formAlert, msg || "Successful", true);
			}
			postAlert("bg-primary-300", msg || "Successful");
			setTimeout(() => {
				if (redirectUrl) {
					window.location.assign(redirectUrl);
				} else {
					window.location.reload();
				}
			}, 1000);
		} else {
			if (formAlert) {
				showError(formAlert, msg || "An error occurred");
			}
			postAlert("bg-secondary-300", msg || "An error occurred");
		}
	} catch (error) {
		if (formAlert) {
			showError(formAlert, error);
		}
		postAlert("bg-secondary-300", error);
	}
}
window.parseXhrResponse = parseXhrResponse;