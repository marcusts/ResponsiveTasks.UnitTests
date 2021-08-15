// *********************************************************************************
// Copyright @2021 Marcus Technical Services, Inc.
// <copyright
// file=Tests.cs
// company="Marcus Technical Services, Inc.">
// </copyright>
// 
// MIT License
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
// *********************************************************************************

namespace ResponsiveTasks.UnitTests
{
   using System;
   using System.Collections.Generic;
   using System.Diagnostics;
   using System.Linq;
   using System.Reflection;
   using System.Threading;
   using System.Threading.Tasks;
   using Bogus;
   using Com.MarcusTS.ResponsiveTasks;
   using Com.MarcusTS.SharedUtils.Interfaces;
   using Com.MarcusTS.SharedUtils.Utils;
   using Microsoft.VisualStudio.TestTools.UnitTesting;

   [TestClass]
   public class Tests
   {
      private const           string        DEFAULT_TEST_PREFIX      = "Test";
      private const           int           MAX_TEST_TASKS           = 5;
      private const           int           SOME_TIME                = 250;
      private const           int           A_LONG_TIME              = SOME_TIME * 4;
      public static readonly  TimeSpan      MAX_DEFAULT_TIME_TO_WAIT = TimeSpan.FromMilliseconds(2500);
      public static readonly  TimeSpan      MIN_DEFAULT_TIME_TO_WAIT = TimeSpan.FromMilliseconds(100);
      private static readonly Faker<object> _faker                   = new();
      private static readonly Random        _random                  = new((int) DateTime.Now.Ticks & 0x0000FFFF);

      static Tests()
      {
         VerifyInterfaceCoverage<IResponsiveTasks, Tests>();
      }

      [TestMethod]
      public Task TestAddIfNotAlreadyThere()
      {
         // Create generic responsive tasks
         var respTask = new ResponsiveTasks();

         // Verify that it has no hosts or members
         Assert.IsTrue(respTask.IsAnEmptyList(),
                       "Newly created responsive tasks has unknown members for no apparent reason");

         // Add ourselves and a fake handler
         respTask.AddIfNotAlreadyThere(this, FakeResponsiveTasksHandler);

         Assert.IsTrue(respTask.IsNotAnEmptyList() && (respTask.Count == 1),
                       "Added a single member to created responsive tasks and ended up with a count of ->" +
                       (respTask.IsAnEmptyList() ? 0 : respTask.Count));

         return Task.CompletedTask;
      }

      [TestMethod]
      public async Task TestAwaitAllTasksCollectively()
      {
         await AwaitAllHandlersUsingRandomTiming(HowToRun.AwaitAllCollectively, "collectively");
         await AwaitAllHandlersUsingTimeoutMilliseconds(HowToRun.AwaitAllCollectively, "collectively");
      }

      [TestMethod]
      public async Task TestAwaitAllTasksConsecutively()
      {
         await AwaitAllHandlersUsingRandomTiming(HowToRun.AwaitAllConsecutively_IgnoreFailures, "consecutively");
         await AwaitAllHandlersUsingTimeoutMilliseconds(HowToRun.AwaitAllConsecutively_IgnoreFailures, "consecutively");
      }

      [TestMethod]
      public async Task TestAwaitAllTasksSafelyFromVoid()
      {
         await AwaitAllHandlersUsingRandomTiming(HowToRun.AwaitAllSafelyFromVoid, "safely from void");
         await AwaitAllHandlersUsingTimeoutMilliseconds(HowToRun.AwaitAllSafelyFromVoid, "safely from void");
      }

      [TestMethod]
      public async Task TestCustomErrorHandler()
      {
         IResponsiveTasksThatOverridesIssueResponsiveError customTasks =
            new ResponsiveTasksThatOverridesIssueResponsiveError();

         // Do nothing
         customTasks.AddIfNotAlreadyThere(this, FakeResponsiveTasksHandler);

         ICustomErrorHandler customErrorHandler = new CustomErrorHandler();
         customTasks.CustomErrorHandler = customErrorHandler;
         customTasks.ParamsErrorLevel   = ParamsErrorLevels.Custom;

         // Force an error by sending in a param when the originally requested count is 0.
         customTasks.RunAllTasksInParallelFromVoid(_faker.Generate());

         await Task.Delay(SOME_TIME);

         Assert.IsTrue(customErrorHandler.ReportedErrorLevel != ParamsErrorLevels.None,
                       "Custom error handler failed to receive a params error");
      }

      [TestMethod]
      public async Task TestParams()
      {
         // Test default params (empty)
         await TestParamDict(default).WithoutChangingContext();
         // Test a few random params
         await TestParamDict(Anything(4)).WithoutChangingContext();

         // PRIVATE METHODS
         async Task TestParamDict(object[] paramValues)
         {
            IResponsiveTasks task = default;

            try
            {
               // Not using indexes for these
               task = new ResponsiveTasks();
            }
            catch (Exception ex)
            {
               Assert.Fail("tried to create a task with params ->" + paramValues + "<- but caught an exception ->" +
                           ex.Message                              + "<-");
            }

            // ELSE
            task.AddIfNotAlreadyThere(this, dict =>
            {
               Debug.Assert(dict.Values.ToArray().IsSameAs(paramValues),
                            "Raised parameters do not match those passed in");
               return Task.CompletedTask;
            });

            // See if the raised Task provides the same params
            await task.AwaitAllTasksConsecutively(paramValues).WithoutChangingContext();
         }
      }

      /// <summary>
      ///    Tests how mismatched param counts get reported. Also verifies that the passed params count matches the
      ///    originally requested param count.
      /// </summary>
      /// <returns></returns>
      [TestMethod]
      public async Task TestParamsErrorLevel()
      {
         const int MAX_TEST_PARAM_COUNTS = 5;

         for (var requestedParamCount = 0; requestedParamCount < MAX_TEST_PARAM_COUNTS; requestedParamCount++)
         {
            // NOTE Random never hits the upper bound, so increasing by 1
            var publishedParamCount = _random.Next(0, MAX_TEST_PARAM_COUNTS + 1);

            foreach (ParamsErrorLevels errorLevel in Enum.GetValues(typeof(ParamsErrorLevels)))
            {
               IResponsiveTasksThatOverridesIssueResponsiveError tasks =
                  new ResponsiveTasksThatOverridesIssueResponsiveError(requestedParamCount)
                  { ParamsErrorLevel = errorLevel };

               // The task doesn't do anything
               tasks.AddIfNotAlreadyThere(this, FakeResponsiveTasksHandler);

               var publishedParams = Anything(publishedParamCount);

               // Run quick and disconneced so we can analyze the results immediately
               tasks.RunAllTasksInParallelFromVoid(publishedParams);

               await Task.Delay(SOME_TIME);

               if (requestedParamCount == publishedParamCount)
               {
                  // No error expected
                  Assert.IsTrue(tasks.ReportedErrorLevel == ParamsErrorLevels.None,
                                "Param counts matched but got a param error ->" +
                                tasks.ReportedErrorLevel                        +
                                "<- and string =>"                              +
                                tasks.ReportedErrorStr                          +
                                "<-");
               }
               else
               {
                  // Error expected
                  Assert.IsTrue(tasks.ReportedErrorLevel == errorLevel,
                                "Params mis-matched.  Expecting error level =>" +
                                errorLevel                                      +
                                "<- but got =>"                                 +
                                tasks.ReportedErrorLevel                        +
                                "<-");
               }
            }
         }
      }

      [TestMethod]
      public Task TestRemoveIfThere()
      {
         // Create generic responsive tasks
         var respTask = new ResponsiveTasks();

         respTask.AddIfNotAlreadyThere(this, FakeResponsiveTasksHandler);

         Assert.IsTrue(respTask.IsNotAnEmptyList() && (respTask.Count == 1),
                       "Added a single member to created responsive tasks and ended up with a count of ->" +
                       (respTask.IsAnEmptyList() ? 0 : respTask.Count));

         respTask.RemoveIfThere(this, FakeResponsiveTasksHandler);

         Assert.IsTrue(respTask.IsAnEmptyList(), "Could not remove a task handler");

         return Task.CompletedTask;
      }

      /// <remarks>Can only verify that all tasks were started -- this approach des not wait for anything to conclude</remarks>
      [TestMethod]
      public async Task TestRunAllTasksInParallelFromVoid()
      {
         var task = new ResponsiveTasks();

         AddHandlersAndSetAllAreRunning<IHandlerWithExpiration, HandlerWithExpiration>(task);

         task.RunAllTasksInParallelFromVoid();

         // Verify that all tasks are still running
         var handlers = task.OfType<IHandlerWithExpiration>().ToArray();

         if (handlers.IsAnEmptyList())
         {
            Debug.WriteLine("No handlers of type ->" + nameof(IHandlerWithExpiration) + "<-");
            return;
         }

         foreach (var handler in handlers)
         {
            Assert.IsTrue(handler.IsRunning.IsTrue(), "Handler has stopped running before cancellation");
         }


         // ELSE ready to start this test

         // Cancel all tasks
         foreach (var handler in handlers)
         {
            handler.CancelTokenSource.Cancel();
         }

         await Task.Delay(A_LONG_TIME);

         // Verify that no task is still running
         foreach (var handler in handlers)
         {
            Assert.IsTrue(handler.IsRunning.IsFalse(), "Handler running after cancellation");
         }
      }

      [TestMethod]
      public Task TestRunAllTasksUsingDefaults()
      {
         // HACK Redundant
         return TestRunHowAndRunAllTasksUsingDefaults();
      }

      [TestMethod]
      public Task TestRunHow()
      {
         // HACK Redundant
         return TestRunHowAndRunAllTasksUsingDefaults();
      }

      [TestMethod]
      public async Task TestTaskErrorBroadcaster()
      {
         IResponsiveTasksThatOverridesIssueResponsiveError customTasks =
            new ResponsiveTasksThatOverridesIssueResponsiveError();

         // Throw an error deliberately
         customTasks.AddIfNotAlreadyThere(this, _ => throw new Exception());

         ICustomErrorBroadcaster customErrorBroadcaster = new CustomErrorBroadcaster();
         customTasks.TaskErrorBroadcaster = customErrorBroadcaster;

         // The task will run and throw its own error
         customTasks.RunAllTasksInParallelFromVoid(_faker.Generate());

         await Task.Delay(SOME_TIME);

         Assert.IsTrue(customErrorBroadcaster.ReportedErrorLevel != ParamsErrorLevels.None,
                       "Custom task broadcaster failed to report a task error");
      }

      /// <remarks>
      ///    Tests that these got set as expected. Actual timeout tests -- verifying tha the timeout occurs -- occur
      ///    throughout this set of unit tests. The number of milliseconds is not critical enough to warrant a special test
      ///    for the count.
      /// </remarks>
      /// <returns></returns>
      [TestMethod]
      public Task TestTimeoutMilliseconds()
      {
         const int MAX_TESTS = 5;

         var tasks = new ResponsiveTasks();

         for (var idx = 0; idx < MAX_TESTS; idx++)
         {
            var numberToTest = _random.Next();
            tasks.TimeoutMilliseconds = numberToTest;

            Assert.IsTrue(tasks.TimeoutMilliseconds == numberToTest, "Assigned time-out milliseconds got lost");
         }

         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestUnsubscribeHost()
      {
         var tasks = new ResponsiveTasks();

         // Add the fake handler
         tasks.AddIfNotAlreadyThere(this, FakeResponsiveTasksHandler);

         Assert.IsTrue(tasks.IsNotAnEmptyList(), "Failed to add a task in preparation for a unit test");

         // Remove ourselves globally
         tasks.UnsubscribeHost(this);

         Assert.IsTrue(tasks.IsAnEmptyList(),
                       "Failed to remove a task handler using the global call to " +
                       nameof(ResponsiveTasks.UnsubscribeHost));

         return Task.CompletedTask;
      }

      private static void VerifyInterfaceCoverage<InterfaceT, TestTypeT>(string testPrefix = DEFAULT_TEST_PREFIX)
      {
         var BINDING_FLAGS = BindingFlags.Public | BindingFlags.FlattenHierarchy | BindingFlags.Instance;

         var testableInterfaceMethods =
            typeof(InterfaceT).GetMethods(BINDING_FLAGS).Where(method => !method.IsSpecialName);
         var testableInterfaceProperties = typeof(InterfaceT).GetProperties(BINDING_FLAGS);

         var existingUnitTestNames = typeof(TestTypeT).GetMembers(BINDING_FLAGS)
                                                      .Where(propInfo => propInfo.Name.StartsWith(DEFAULT_TEST_PREFIX))
                                                      .Select(propInfo => propInfo.Name).ToArray();

         foreach (var testableInterfaceMethod in testableInterfaceMethods)
         {
            if (!existingUnitTestNames.Contains(DEFAULT_TEST_PREFIX + testableInterfaceMethod.Name))
            {
               Debug.WriteLine(nameof(Tests)                + ": Interface ->" + nameof(InterfaceT) + "<- method ->" +
                               testableInterfaceMethod.Name + "<- is not unit tested !!! ");
            }
         }

         foreach (var testableInterfaceProperty in testableInterfaceProperties)
         {
            var baseName = testableInterfaceProperty.Name.Replace("get_", "").Replace("set_", "");

            if (!existingUnitTestNames.Contains(DEFAULT_TEST_PREFIX + baseName))
            {
               Debug.WriteLine(nameof(Tests) + ": Interface ->" + nameof(InterfaceT) + "<- property ->" +
                               baseName      + "<- is not unit tested !!! ");
            }
         }
      }

      private void AddHandlersAndSetAllAreRunning<InterfaceT, ClassT>(IResponsiveTasks targetTask, int count = MAX_TEST_TASKS)
         where InterfaceT : IHandlerWithExpiration
         where ClassT : HandlerWithExpiration, InterfaceT
      {
         for (var idx = 0; idx < count; idx++)
         {
            InterfaceT handlerHost = Activator.CreateInstance<ClassT>();
            targetTask.AddIfNotAlreadyThere(this, paramDict => handlerHost.HandlerThatExpires(paramDict));
         }
      }

      private void AddHandlersAndSetFixedTimeout(IResponsiveTasks targetTask, int timeToWaitMillisecdonds, int count = MAX_TEST_TASKS)
      {
         for (var idx = 0; idx < count; idx++)
         {
            IHandlerWithExpiration handlerHost = new HandlerWithExpiration()
                                                 { TimeToWait = TimeSpan.FromMilliseconds(timeToWaitMillisecdonds) };
            targetTask.AddIfNotAlreadyThere(this, paramDict => handlerHost.HandlerThatExpires(paramDict));
         }
      }

      private object[] Anything(int count)
      {
         var retList = new List<object>();

         for (var idx = 0; idx < count; idx++)
         {
            retList.Add(_faker.Generate());
         }

         return retList.ToArray<object>();
      }

      private async Task RunTaskForAllHandlers(IResponsiveTasks task, HowToRun runHow)
      {
         switch (runHow)
         {
            case HowToRun.AwaitAllConsecutively_IgnoreFailures:
            case HowToRun.AwaitAllConsecutively_StopOnFirstFailure:
               await task.AwaitAllTasksConsecutively(default);
               break;

            case HowToRun.AwaitAllCollectively:
               await task.AwaitAllTasksCollectively(default);
               break;

            case HowToRun.AwaitAllSafelyFromVoid:
               task.AwaitAllTasksSafelyFromVoid(default);
               break;

            default:
               Debug.WriteLine(nameof(RunTaskForAllHandlers) + ": illegal run request ->" + runHow + "<-");
               break;
         }
      }

      private async Task AwaitAllHandlersUsingTimeoutMilliseconds(HowToRun runHow, string errorText)
      {
         var task = new ResponsiveTasks();

         // Set this very early
         task.TimeoutMilliseconds = 100;

         // Set this after the timeout
         AddHandlersAndSetFixedTimeout(task, task.TimeoutMilliseconds * 10);

         await RunTaskForAllHandlers(task, runHow);

         // Wait for the pre-set timeout on the parent task
         // The handlers will continue running but should stop when they hit this timeout
         await Task.Delay(task.TimeoutMilliseconds).WithoutChangingContext();

         // Verify that all handlers have completed
         foreach (var handler in task.OfType<IHandlerWithExpiration>())
         {
            Assert.IsTrue(handler.IsRunning.IsFalse(),
                          "Timed out all handlers " + errorText + " but afterwards at least one handler was still running");
         }
      }

      private async Task AwaitAllHandlersUsingRandomTiming(HowToRun runHow, string errorText)
      {
         var task = new ResponsiveTasks();
         AddHandlersAndSetAllAreRunning<IHandlerWithRandomExpiration, HandlerWithRandomExpiration>(task);


         await RunTaskForAllHandlers(task, runHow);

         // Verify that all handlers have completed
         foreach (var handler in task.OfType<IHandlerWithExpiration>())
         {
            Assert.IsTrue(handler.IsRunning.IsFalse(),
                          "Awaited completion of all handlers " + errorText + " but afterwards at least one handler was still running");
         }
      }

      /*
      [TestMethod]
      public async Task TestIsRunning()
      {
         var task = new ResponsiveTasks();
         IHandlerThatWaitsForCancellation cancellableTask = new HandlerThatWaitsForCancellation();
         task.AddIfNotAlreadyThere(this, cancellableTask.TaskThatWaitsForCancellation);

         // If run parallel, sets IsRunning off immediately. If awaited, hangs because we are running but stuck inside
         // the await.
         ???
         await task.AwaitAllTasksConsecutively(default).WithoutChangingContext();

         await Task.Delay(A_LONG_TIME).WithoutChangingContext();

         Assert.IsTrue(task.IsRunning.IsTrue(), "Task was started but IsRunning is false");

         cancellableTask.CancelTokenSource.Cancel();

         await Task.Delay(A_LONG_TIME).WithoutChangingContext();

         Assert.IsTrue(task.IsRunning.IsFalse(), "Cancelling task failed to switch IsRunning off");
      }
      */

      private Task FakeResponsiveTasksHandler(IResponsiveTaskParams paramDict)
      {
         // Does nothing
         return Task.CompletedTask;
      }

      /// <remarks>Two unit tests require the same basic test.</remarks>
      private async Task TestRunHowAndRunAllTasksUsingDefaults()
      {
         IResponsiveTasksThatOverridesRunAllTasksUsingDefaults tasks =
            new ResponsiveTasksThatOverridesRunAllTasksUsingDefaults();

         // Do nothing
         tasks.AddIfNotAlreadyThere(this, FakeResponsiveTasksHandler);

         foreach (HowToRun howToRun in Enum.GetValues(typeof(HowToRun)))
         {
            tasks.LastRunHow = HowToRun.NotSet;
            tasks.RunHow     = howToRun;

            await tasks.RunAllTasksUsingDefaults().WithoutChangingContext();

            Assert.IsTrue(tasks.LastRunHow == howToRun,
                          "Ran tasks using default with how to run ->" + howToRun + "<- but ran with ->" +
                          tasks.LastRunHow                             + "<-");
         }
      }

      private interface ICustomErrorBroadcaster : IIssueResponsiveErrors, IReportErrorStatus
      {
      }

      private interface ICustomErrorHandler : ICustomResponsiveParameterErrorHandler, IReportErrorStatus
      {
      }

      private interface IHandlerWithExpiration : ICanRun
      {
         // Can wait for this time
         TimeSpan TimeToWait { get; set; }

         // Can be canceled
         CancellationTokenSource CancelTokenSource { get; }

         Task                    HandlerThatExpires(IResponsiveTaskParams paramDict);
      }

      private interface IHandlerWithRandomExpiration : IHandlerWithExpiration
      {
      }

      private interface IReportErrorStatus
      {
         ParamsErrorLevels ReportedErrorLevel { get; set; }
         string            ReportedErrorStr   { get; set; }
      }

      private interface IResponsiveTasksThatOverridesIssueResponsiveError : IReportErrorStatus, IResponsiveTasks
      {
      }

      private interface IResponsiveTasksThatOverridesRunAllTasksUsingDefaults : IResponsiveTasks
      {
         HowToRun LastRunHow { get; set; }
      }

      private class CustomErrorBroadcaster : ICustomErrorBroadcaster
      {
         public ParamsErrorLevels ParamsErrorLevel   { get; set; } = ParamsErrorLevels.DebugWriteLine;
         public ParamsErrorLevels ReportedErrorLevel { get; set; } = ParamsErrorLevels.None;
         public string            ReportedErrorStr   { get; set; }

         public void IssueResponsiveError(ParamsErrorLevels paramsErrorLevel, string errorStr)
         {
            ReportedErrorLevel = paramsErrorLevel;
            ReportedErrorStr   = errorStr;
         }
      }

      private class CustomErrorHandler : ICustomErrorHandler
      {
         public ParamsErrorLevels ReportedErrorLevel { get; set; } = ParamsErrorLevels.None;

         public string ReportedErrorStr { get; set; }

         public Task HandleErrorMessage(ParamsErrorLevels paramsErrorLevel, string errorStr)
         {
            ReportedErrorLevel = paramsErrorLevel;
            ReportedErrorStr   = errorStr;

            return Task.CompletedTask;
         }
      }

      private class HandlerWithExpiration : IHandlerWithExpiration
      {
         public IThreadSafeAccessor     IsRunning         { get; }      = new ThreadSafeAccessor();

         // Wait endlessly unless told otherwise
         public TimeSpan                TimeToWait        { get; set; } = TimeSpan.MaxValue;

         // Token can be used when TimeToWait is at its max
         public CancellationTokenSource CancelTokenSource { get; }      = new();

         /// <remarks>Must match the signature of the <see cref="ResponsiveTaskBroadcastDelegate" />.</remarks>
         public async Task HandlerThatExpires(IResponsiveTaskParams paramDict)
         {
            var endDateTime = DateTime.Now + TimeToWait;
            IsRunning.SetTrue();

            while ((DateTime.Now < endDateTime) && !CancelTokenSource.IsCancellationRequested)
            {
               await Task.Delay(SOME_TIME).WithoutChangingContext();
            }

            IsRunning.SetFalse();
         }
      }

      private class HandlerWithRandomExpiration : HandlerWithExpiration, IHandlerWithRandomExpiration
      {
         public HandlerWithRandomExpiration()
         {
            TimeToWait =
               TimeSpan.FromMilliseconds(_random.Next(MIN_DEFAULT_TIME_TO_WAIT.Milliseconds,
                                                      MAX_DEFAULT_TIME_TO_WAIT.Milliseconds + 1));
         }
      }

      private class ResponsiveTasksThatOverridesIssueResponsiveError : ResponsiveTasks,
                                                                       IResponsiveTasksThatOverridesIssueResponsiveError
      {
         public ResponsiveTasksThatOverridesIssueResponsiveError(params object[] paramKeys)
            : base(paramKeys)
         {
         }

         public ResponsiveTasksThatOverridesIssueResponsiveError(int paramCount = 0)
            : base(paramCount)
         {
         }

         public ParamsErrorLevels ReportedErrorLevel { get; set; } = ParamsErrorLevels.None;

         public string ReportedErrorStr { get; set; }

         public override void IssueResponsiveError(ParamsErrorLevels paramsErrorLevel, string errorStr)
         {
            ReportedErrorLevel = paramsErrorLevel;
            ReportedErrorStr   = errorStr;

            // Allow custom to get through (otherwise, can't be unit tested)
            if (ParamsErrorLevel == ParamsErrorLevels.Custom)
            {
               base.IssueResponsiveError(paramsErrorLevel, errorStr);
            }
         }
      }

      private class ResponsiveTasksThatOverridesRunAllTasksUsingDefaults : ResponsiveTasks,
                                                                           IResponsiveTasksThatOverridesRunAllTasksUsingDefaults
      {
         public ResponsiveTasksThatOverridesRunAllTasksUsingDefaults(params object[] paramKeys)
            : base(paramKeys)
         {
         }

         public ResponsiveTasksThatOverridesRunAllTasksUsingDefaults(int paramCount = 0)
            : base(paramCount)
         {
         }

         public HowToRun LastRunHow { get; set; } = HowToRun.NotSet;

         protected override Task<bool> RunAllTasksUsingDefaults_Internal(HowToRun runHow, params object[] paramValues)
         {
            LastRunHow = runHow;
            return Task.FromResult(true);
         }
      }
   }
}