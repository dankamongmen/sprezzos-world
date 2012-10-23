/*
 * exec.c
 *
 * Copyright (C) 2003 Bastian Blank <waldi@debian.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <debian-installer/exec.h>

#include <debian-installer/log.h>

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/poll.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define MAXLINE 1024

#define EX_NOEXEC       126
#define EX_NOTFOUND     127

static int internal_di_exec (const char *path, bool use_path, const char *const argv[], const char *const envp[], di_io_handler *stdout_handler, di_io_handler *stderr_handler, void *io_user_data, di_process_handler *parent_prepare_handler, void *parent_prepare_user_data, di_process_handler *child_prepare_handler, void *child_prepare_user_data)
{
  char line[MAXLINE];
  pid_t pid, pid2;
  int fds[4] = { -1, }, mode = 0, pipes = 0, i;

  if (stdout_handler)
  {
    mode += 1;
    pipes++;
  }
  if (stderr_handler)
  {
    mode += 2;
    pipes++;
  }

  for (i = 0; i < pipes; i++)
    pipe (&fds[i * 2]);

  pid = fork ();

  if (pid <= 0)
  {
    for (i = 0; i < pipes; i++)
      close (fds[i * 2]);
  }

  if (pid == 0)
  {
    int temp, realfds[3] =
    {
      0,
      fds[1],
      fds[3]
    };

    temp = open ("/dev/null", O_RDWR);

    switch (mode)
    {
      case 0:
        realfds[1] = temp;
        realfds[2] = temp;
        break;
      case 2:
        realfds[1] = temp;
        realfds[2] = fds[1];
        break;
      case 1:
        realfds[2] = fds[1];
        break;
    }

    for (i = 1; i <= 2; i++)
      dup2 (realfds[i], i);

    close (temp);
  }

  for (i = 0; i < pipes; i++)
    close (fds[i * 2 + 1]);

  if (pid == 0)
  {
    if (child_prepare_handler)
      if (child_prepare_handler (pid, child_prepare_user_data))
        exit (255);

    if (use_path)
      execvp (path, (char *const *) argv);
    else if (envp)
      execve (path, (char *const *) argv, (char *const *) envp);
    else
      execv (path, (char *const *) argv);
    exit (errno == ENOENT ? EX_NOTFOUND : EX_NOEXEC);
  }
  else if (pid < 0)
  {
    di_log (DI_LOG_LEVEL_WARNING, "fork failed");
    return -1;
  }
  else
  {
    int status = -1;
    struct pollfd pollfds[pipes];
    struct files
    {
      FILE *file;
      di_io_handler *handler;
    }
    files[pipes];

    for (i = 0; i < pipes; i++)
    {
      fcntl (fds[i * 2], F_SETFL, O_NONBLOCK);
      files[i].file = fdopen (fds[i * 2], "r");
      pollfds[i].fd = fds[i * 2];
      pollfds[i].events = POLLIN;
    }

    switch (mode)
    {
      case 2:
        files[0].handler = stderr_handler;
        break;
      case 3:
        files[1].handler = stderr_handler;
      case 1:
        files[0].handler = stdout_handler;
        break;
    }

    if (parent_prepare_handler && parent_prepare_handler (pid, parent_prepare_user_data))
      kill (pid, 9);
    else if (pipes)
      while (poll (pollfds, pipes, -1) >= 0)
      {
        bool exit = false;

        /* Implementations of poll() deliver various combinations of POLLIN and
           POLLHUP on EOF. fgets() detects it and we check with feof() below.
           References: http://www.greenend.org.uk/rjk/2001/06/poll.html */
        for (i = 0; i < pipes; i++)
        {
          if (pollfds[i].revents & (POLLIN | POLLHUP))
          {
            while (fgets (line, sizeof (line), files[i].file) != NULL)
            {
              size_t len = strlen (line);
              if (line[len - 1] == '\n')
              {
                line[len - 1] = '\0';
                len--;
              }
              files[i].handler (line, len, io_user_data);
              exit = true;
            }
          }
        }

        if (exit)
          continue;

        for (i = 0; i < pipes; i++)
          if (feof(files[i].file))
            exit = true;

        if (exit)
          break;
      }

    /* waitpid() can be interrupted by the SIGCHLD setup in log-output
       in case of a working poll() implementation. This depends on the
       kernel and the scheduler. */
    do 
    {
      pid2 = waitpid (pid, &status, 0);
    } while (pid2 == -1 && errno == EINTR);

    if (!pid2)
      return -1;
 
    for (i = 0; i < pipes; i++)
      fclose (files[i].file); /* closes fds[i * 2] */

    return status;
  }

  return -1;
}

int di_exec_full (const char *path, const char *const argv[], di_io_handler *stdout_handler, di_io_handler *stderr_handler, void *io_user_data, di_process_handler *parent_prepare_handler, void *parent_prepare_user_data, di_process_handler *child_prepare_handler, void *child_prepare_user_data)
{
  return internal_di_exec (path, false, argv, NULL, stdout_handler, stderr_handler, io_user_data, parent_prepare_handler, parent_prepare_user_data, child_prepare_handler, child_prepare_user_data);
}

int di_exec_env_full (const char *path, const char *const argv[], const char *const envp[], di_io_handler *stdout_handler, di_io_handler *stderr_handler, void *io_user_data, di_process_handler *parent_prepare_handler, void *parent_prepare_user_data, di_process_handler *child_prepare_handler, void *child_prepare_user_data)
{
  return internal_di_exec (path, false, argv, envp, stdout_handler, stderr_handler, io_user_data, parent_prepare_handler, parent_prepare_user_data, child_prepare_handler, child_prepare_user_data);
}

int di_exec_path_full (const char *file, const char *const argv[], di_io_handler *stdout_handler, di_io_handler *stderr_handler, void *io_user_data, di_process_handler *parent_prepare_handler, void *parent_prepare_user_data, di_process_handler *child_prepare_handler, void *child_prepare_user_data)
{
  return internal_di_exec (file, true, argv, NULL, stdout_handler, stderr_handler, io_user_data, parent_prepare_handler, parent_prepare_user_data, child_prepare_handler, child_prepare_user_data);
}

int di_exec_shell_full (const char *const cmd, di_io_handler *stdout_handler, di_io_handler *stderr_handler, void *io_user_data, di_process_handler *parent_prepare_handler, void *parent_prepare_user_data, di_process_handler *child_prepare_handler, void *child_prepare_user_data)
{
  const char *const argv[] = { "sh", "-c", cmd, NULL };
  return internal_di_exec ("/bin/sh", false, argv, NULL, stdout_handler, stderr_handler, io_user_data, parent_prepare_handler, parent_prepare_user_data, child_prepare_handler, child_prepare_user_data); 
}

int di_exec_prepare_chdir (pid_t pid __attribute__ ((unused)), void *user_data)
{
  char *path = user_data;
  if (chdir (path))
    return -1;
  return 0;
}

int di_exec_prepare_chroot (pid_t pid __attribute__ ((unused)), void *user_data)
{
  char *path = user_data;
  if (chroot (path))
    return -1;
  if (chdir ("/"))
    return -1;
  return 0;
}

int di_exec_io_log (const char *buf, size_t len __attribute__ ((unused)), void *user_data __attribute__ ((unused)))
{
  di_log (DI_LOG_LEVEL_OUTPUT, "%s", buf);
  return 0;
}

int di_exec_mangle_status (int status)
{
  if (WIFEXITED (status))
    return WEXITSTATUS (status);
  if (WIFSIGNALED (status))
    return 128 + WTERMSIG (status);
  if (WIFSTOPPED (status))
    return 128 + WSTOPSIG (status);
  return status;
}

