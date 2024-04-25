// fetchAndDisplayInfo();
// const pollingInterval = 3000;
let previousContent = [];
// setInterval(fetchAndDisplayInfo, pollingInterval);

async function fetchUnikernelInfo() {
  try {
    const response = await fetch("/unikernel-info");
    const data = await response.json();
    console.log("Fetched content:", data);
    return data;
  } catch (error) {
    console.error("Error fetching content:", error);
    return [];
  }
}


async function fetchAndDisplayInfo() {
  const content = await fetchUnikernelInfo();
  const unikernelContainer = document.getElementById("unikernel-container");
  const emptyMessage = document.getElementById("no-molly");

  if (content.length === 0) {
    emptyMessage.classList.remove("hidden");
    emptyMessage.classList.add("block");
  } else {
    content.forEach(async item => {
      if (!previousContent.some(prevItem => prevItem.name === item.name)) {
        const unikernelDiv = document.createElement("div");
        unikernelDiv.classList.add("pb-1", "w-full", "rounded-md", "hover:scale-105", "cursor-pointer", "shadow-md", "bg-gray-900");

        const contentDiv = document.createElement("div")
        contentDiv.classList.add("text-left", "text-white");
        const title = document.createElement("h2");
        const type = document.createElement("p");
        const f_behaviour = document.createElement("p");
        const cpu = document.createElement("p");
        const memory = document.createElement("p");
        title.classList.add("text-xl", "font-bold");
        title.textContent = "Unikernel: " + item.name.substring(1);
        type.textContent = "Type: " + item.type;
        f_behaviour.textContent = "Fail behavious: " + item.fail_behaviour;
        cpu.textContent = "CPU ID: " + item.cpuid;
        memory.textContent = "Memory: " + item.memory;

        contentDiv.appendChild(title);
        contentDiv.appendChild(type);
        contentDiv.appendChild(f_behaviour);
        contentDiv.appendChild(cpu);
        contentDiv.appendChild(memory);
        unikernelDiv.appendChild(contentDiv);
        unikernelContainer.appendChild(unikernelDiv);
      }
    });
  }

  previousContent = content;
}


document.addEventListener("DOMContentLoaded", function() {
  fetchAndDisplayInfo()
});
