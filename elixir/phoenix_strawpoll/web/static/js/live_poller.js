import { Socket } from "deps/phoenix/web/static/js/phoenix";

export class LivePoller {
  constructor() {
    if (!$("#poll-id").length) { return; }
    const pollChannel = this._setupPollChannel();
    this._setupVoteButtons(pollChannel);
    this._setupGraph();
  }

  _createSocket() {
    const socket = new Socket("/socket");
    socket.connect();
    socket.onOpen(() => console.log("Connected"));
    return socket;
  }

  _setupPollChannel() {
    const socket = this._createSocket();
    const pollId = $("#poll-id").val();
    const pollChannel = socket.channel("polls:" + pollId);
    pollChannel.on("new_vote", vote => {
      this._updateDisplay(vote.entry_id);
      this._updateGraph();
    });
    pollChannel.on("close", vote => {
      if (status.closed) {
        $("a.vote").addClass("hidden")
          $("#poll-closed").text("false")
      } else {
        $("a.vote").removeClass("hidden")
          $("#poll-closed").text("true")
      }
    });
    pollChannel
        .join()
        .receive("ok", resp => { console.log("Joined") })
        .receive("error", reason => { console.log("Error: ", reason) });
    return pollChannel;
  }

  _updateDisplay(entryId) {
    $.each($("li.entry"), (index, item) => {
      const li = $(item);
      if (entryId == li.data("entry-id")) {
        const newVotes = +(li.find(".votes").text()) + 1;
        this._updateEntry(li, newVotes);
      }
    });
  }

  _updateEntry(li, newVotes) {
    li.find(".votes").text(newVotes);
  }

  _setupVoteButtons(pollChannel) {
    $(".vote").on("click", event => {
      event.preventDefault();
      const li = $(event.currentTarget).parents("li");
      const entry_id = li.data("entry-id");
      const pollId = $("#poll-id").val();
      pollChannel.push("new_vote", { entry_id });
    });
  }

  _setupGraph() {
    google.load("visualization", "1", { packages: ["corechart"] });
    google.setOnLoadCallback(() => {
      this.chart = new google.visualization.PieChart(document.getElementById("my-chart"));
      this._updateGraph();
    });
  }

  _updateGraph() {
    const data = this._getGraphData();
    const convertedData = google.visualization.arrayToDataTable(data);
    this.chart.draw(convertedData, { title: "Poll", is3D: true });
  }

  _getGraphData() {
    const data = [["Choice", "Votes"]];
    $.each($("li.entry"), (index, item) => {
      const li = $(item);
      const title = li.find(".title").text();
      const votes = +(li.find(".votes").text());
      data.push([title, votes]);
    });
    return data;
  }
}
