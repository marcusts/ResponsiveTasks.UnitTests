namespace ResponsiveTasks.UnitTests
{
   using Com.MarcusTS.ResponsiveTasks;
   using Microsoft.VisualStudio.TestTools.UnitTesting;
   using System.Diagnostics;
   using System.Linq;
   using System.Reflection;
   using System.Threading.Tasks;

   [TestClass]
   public class Tests
   {
      private const string DEFAULT_TEST_PREFIX = "Test";

      static Tests()
      {
         VerifyInterfaceCoverage<IResponsiveTasks, Tests>();
      }

      [TestMethod]
      public Task TestAddIfNotAlreadyThere()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestAwaitAllTasksCollectively()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestAwaitAllTasksConsecutively()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestConstruction()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestErrorHandling()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestIsRunning()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestParams()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestParamsErrorLevel()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestRemoveIfThere()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestRunAllTasksInParallelFromVoid()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestRunAllTasksUsingDefaults()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestRunHow()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestTaskErrorBroadcaster()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestTestCustomErrorHandler()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestTimeout()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestTimeoutMilliseconds()
      {
         return Task.CompletedTask;
      }

      [TestMethod]
      public Task TestUnsubscribeHost()
      {
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
               Debug.WriteLine(nameof(Tests) + ": Interface ->" + nameof(InterfaceT) + "<- method ->" +
                               testableInterfaceMethod.Name + "<- is not unit tested !!! ");
            }
         }

         foreach (var testableInterfaceProperty in testableInterfaceProperties)
         {
            var baseName = testableInterfaceProperty.Name.Replace("get_", "").Replace("set_", "");

            if (!existingUnitTestNames.Contains(DEFAULT_TEST_PREFIX + baseName))
            {
               Debug.WriteLine(nameof(Tests) + ": Interface ->" + nameof(InterfaceT) + "<- property ->" +
                               baseName + "<- is not unit tested !!! ");
            }
         }
      }
   }
}
