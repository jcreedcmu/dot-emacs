#!/usr/bin/env node

// usage:
// ./change-workspace.js 1 # go to next workspace
// ./change-workspace.js -1 # go to prev workspace
const util = require('util');
const exec = util.promisify(require('child_process').execFile);

async function get_workspaces() {
  const { stdout, stderr } = await exec('i3-msg', ['-t', 'get_workspaces']);
  if (stderr.length > 0) {
	 throw new Error(stderr);
  }
  return JSON.parse(stdout);
}

async function get_current_workspace() {
  const workspaces = await get_workspaces(); // XXX cache
  return workspaces.find(w => w.focused).num;
}

async function go(delta) {
  const next_workspace =
		  Math.max(MIN_WORKSPACE,
					  Math.min(MAX_WORKSPACE, await get_current_workspace() + delta));
  exec('i3-msg', ['workspace', next_workspace]);
}

go(parseInt(process.argv[2] ?? 0));
