import unittest
import asyncio
from unittest.mock import patch, MagicMock
from io import StringIO
import sys

from chat import Order, CoffeeShopEvents, guest_task, employee_task, MENU


class TestOrder(unittest.TestCase):
    """Test the Order class functionality"""

    def setUp(self):
        self.order = Order()

    def test_add_item(self):
        """Test adding items to an order"""
        self.order.add_item("americano")
        self.assertEqual(self.order.items, {"americano": 1})

        # Add the same item again
        self.order.add_item("americano")
        self.assertEqual(self.order.items, {"americano": 2})

        # Add a different item
        self.order.add_item("cookie")
        self.assertEqual(self.order.items, {"americano": 2, "cookie": 1})

        # Add non-menu item (should not be added)
        self.order.add_item("sandwich")
        self.assertEqual(self.order.items, {"americano": 2, "cookie": 1})

    def test_remove_item(self):
        """Test removing items from an order"""
        # Add items to remove
        self.order.add_item("latte")
        self.order.add_item("cookie")
        self.order.add_item("cookie")

        # Remove one cookie
        result = self.order.remove_item("cookie")
        self.assertTrue(result)
        self.assertEqual(self.order.items, {"latte": 1, "cookie": 1})

        # Remove non-existent item
        result = self.order.remove_item("espresso")
        self.assertFalse(result)

        # Remove last of an item
        result = self.order.remove_item("cookie")
        self.assertTrue(result)
        self.assertEqual(self.order.items, {"latte": 1})

        # Remove when item is now gone
        result = self.order.remove_item("cookie")
        self.assertFalse(result)

    def test_calculate_total(self):
        """Test calculating the total price of an order"""
        self.assertEqual(self.order.calculate_total(), 0.0)

        self.order.add_item("americano")  # $1.51
        self.assertAlmostEqual(self.order.calculate_total(), 1.51)

        self.order.add_item("cookie")     # $0.50
        self.assertAlmostEqual(self.order.calculate_total(), 2.01)

        self.order.add_item("latte")      # $2.13
        self.assertAlmostEqual(self.order.calculate_total(), 4.14)

        # Remove an item
        self.order.remove_item("americano")
        self.assertAlmostEqual(self.order.calculate_total(), 2.63)

    def test_has_cookie(self):
        """Test checking if an order has a cookie"""
        self.assertFalse(self.order.has_cookie())

        self.order.add_item("americano")
        self.assertFalse(self.order.has_cookie())

        self.order.add_item("cookie")
        self.assertTrue(self.order.has_cookie())

        self.order.remove_item("cookie")
        self.assertFalse(self.order.has_cookie())

    def test_has_drink(self):
        """Test checking if an order has a drink"""
        self.assertFalse(self.order.has_drink())

        self.order.add_item("cookie")
        self.assertFalse(self.order.has_drink())

        self.order.add_item("americano")
        self.assertTrue(self.order.has_drink())

        self.order.remove_item("americano")
        self.assertFalse(self.order.has_drink())

        # Test with different drinks
        drinks = ["tea", "latte", "espresso", "macchiato"]
        for drink in drinks:
            self.order = Order()  # Reset order
            self.order.add_item(drink)
            self.assertTrue(self.order.has_drink())


class TestCoffeeShopEvents(unittest.TestCase):
    """Test the CoffeeShopEvents class"""

    def setUp(self):
        self.events = CoffeeShopEvents()

    def test_initialization(self):
        """Test proper initialization of events and queues"""
        # Check that all events are initialized
        self.assertFalse(self.events.guest_arrived.is_set())
        self.assertFalse(self.events.order_taken.is_set())
        self.assertFalse(self.events.coffee_ready.is_set())
        self.assertFalse(self.events.transaction_complete.is_set())

        # Check that queues are empty
        self.assertTrue(self.events.guest_messages.empty())
        self.assertTrue(self.events.employee_messages.empty())

        # Check that order is initialized
        self.assertIsInstance(self.events.shared_order, Order)
        self.assertEqual(self.events.shared_order.items, {})


class TestAsyncFunctions(unittest.IsolatedAsyncioTestCase):
    """Test the async functions"""

    async def asyncSetUp(self):
        self.events = CoffeeShopEvents()

    @patch('sys.stdout', new_callable=StringIO)
    async def test_guest_task_simple(self, mock_stdout):
        """Test guest_task with a simple script"""
        guest_script = [
            "I'd like a latte.",
            "No, thank you.",
            "That's all."
        ]

        # Start guest task
        guest_future = asyncio.create_task(guest_task(self.events, guest_script))

        # Simulate employee responses
        await self.events.guest_arrived.wait()
        await self.events.employee_messages.put("Welcome to our coffee shop. What can I get you?")

        # First message from guest
        msg = await self.events.guest_messages.get()
        self.assertEqual(msg, "I'd like a latte.")
        await self.events.employee_messages.put("Would you like to add a cookie for $0.5?")

        # Second message from guest
        msg = await self.events.guest_messages.get()
        self.assertEqual(msg, "No, thank you.")
        await self.events.employee_messages.put("Would you like anything else?")

        # Third message from guest
        msg = await self.events.guest_messages.get()
        self.assertEqual(msg, "That's all.")
        await self.events.employee_messages.put("Your total is $2.13. Thank you and have a nice day!")

        # Signal coffee is ready
        await asyncio.sleep(0.1)  # Small delay to ensure order_taken is set
        self.events.coffee_ready.set()

        # Wait for guest task to complete
        await guest_future

        # Check that transaction is complete
        self.assertTrue(self.events.transaction_complete.is_set())

    @patch('sys.stdout', new_callable=StringIO)
    async def test_employee_task_basic(self, mock_stdout):
        """Test basic employee task behaviors"""
        # Start employee task
        employee_future = asyncio.create_task(employee_task(self.events))

        # Simulate guest arrival
        self.events.guest_arrived.set()

        # Check employee greeting
        greeting = await self.events.employee_messages.get()
        self.assertEqual(greeting, "Welcome to our coffee shop. What can I get you?")

        # Simulate guest ordering
        await self.events.guest_messages.put("I'd like an americano.")
        response = await self.events.employee_messages.get()
        self.assertEqual(response, "Would you like to add a cookie for $0.5?")

        # Decline cookie
        await self.events.guest_messages.put("No, thank you.")
        response = await self.events.employee_messages.get()
        self.assertEqual(response, "Would you like anything else?")

        # Finish order
        await self.events.guest_messages.put("That's all.")
        response = await self.events.employee_messages.get()
        self.assertEqual(response, "Your total is $1.51. Thank you and have a nice day!")

        # Mark order as taken
        self.events.order_taken.set()

        # Instead of waiting a fixed time, either:
        # 1. Set the coffee_ready event ourselves for testing purposes:
        self.events.coffee_ready.set()

        # OR 2. Wait with a timeout for the coffee to be ready
        # try:
        #     await asyncio.wait_for(self.events.coffee_ready.wait(), timeout=6.0)
        #     self.assertTrue(self.events.coffee_ready.is_set())
        # except asyncio.TimeoutError:
        #     self.fail("Coffee ready event was not set within the timeout period")

        # Complete transaction
        self.events.transaction_complete.set()

        # Cancel employee task
        employee_future.cancel()
        try:
            await employee_future
        except asyncio.CancelledError:
            pass

    @patch('sys.stdout', new_callable=StringIO)
    async def test_employee_responses(self, mock_stdout):
        """Test various employee responses to different inputs"""
        # Start employee task
        employee_future = asyncio.create_task(employee_task(self.events))

        # Simulate guest arrival
        self.events.guest_arrived.set()
        await self.events.employee_messages.get()  # Get initial greeting

        # Test response to unknown command
        await self.events.guest_messages.put("What time do you close?")
        response = await self.events.employee_messages.get()
        self.assertEqual(response, "I don't understand.")

        # Test ordering an item
        await self.events.guest_messages.put("I'd like a tea.")
        response = await self.events.employee_messages.get()
        self.assertEqual(response, "Would you like to add a cookie for $0.5?")

        # Test accepting cookie upsell
        await self.events.guest_messages.put("Yes, please.")
        response = await self.events.employee_messages.get()
        self.assertEqual(response, "Would you like anything else?")

        # Test removing an item
        await self.events.guest_messages.put("I don't want a cookie.")
        response = await self.events.employee_messages.get()
        self.assertEqual(response, "Would you like anything else?")

        # Test let me think response
        await self.events.guest_messages.put("Let me think.")
        response = await self.events.employee_messages.get()
        self.assertEqual(response, "Please let me know when you are ready.")

        # Complete order
        await self.events.guest_messages.put("That's all.")
        response = await self.events.employee_messages.get()

        # Set order taken and clean up
        self.events.order_taken.set()
        self.events.transaction_complete.set()
        employee_future.cancel()

        # Check order contents
        self.assertEqual(self.events.shared_order.items, {"tea": 1})


class TestIntegration(unittest.IsolatedAsyncioTestCase):
    """Test the integration between guest and employee tasks"""
    @patch('sys.stdout', new_callable=StringIO)
    async def test_full_interaction(self, mock_stdout):
        """Test a full interaction between guest and employee"""
        events = CoffeeShopEvents()

        # Simpler script that should work more reliably
        guest_script = [
            "I'd like an espresso.",
            "Yes, please.",  # Accept cookie
            "That's all."
        ]

        # Run both tasks concurrently
        await asyncio.gather(
            guest_task(events, guest_script),
            employee_task(events)
        )

        # Check the final order - revised expectations
        self.assertEqual(events.shared_order.items, {"espresso": 1, "cookie": 1})
        expected_total = MENU["espresso"] + MENU["cookie"]
        self.assertAlmostEqual(events.shared_order.calculate_total(), expected_total)

        # Verify all events were set
        self.assertTrue(events.guest_arrived.is_set())
        self.assertTrue(events.order_taken.is_set())
        self.assertTrue(events.coffee_ready.is_set())
        self.assertTrue(events.transaction_complete.is_set())


if __name__ == '__main__':
    unittest.main()
