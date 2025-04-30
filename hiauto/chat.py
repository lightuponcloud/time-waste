import asyncio
import sys
from typing import Dict, List


MENU = {
    "americano": 1.51,
    "espresso": 1.27,
    "latte": 2.13,
    "macchiato": 3.39,
    "tea": 1.75,
    "cookie": 0.50
}


class Order:
    """
    Helper class, stores coffe shop order related attributes.
    """
    def __init__(self):
        self.items: Dict[str, int] = {}  # Item name to quantity

    def add_item(self, item: str) -> None:
        """
        Add item to order
        """
        if item in MENU:
            if item in self.items:
                self.items[item] += 1
            else:
                self.items[item] = 1

    def remove_item(self, item: str) -> bool:
        """
        Remove item from order
        """
        if item in self.items and self.items[item] > 0:
            self.items[item] -= 1
            if self.items[item] == 0:
                del self.items[item]
            return True
        return False

    def calculate_total(self) -> float:
        """
        Return total value of order
        """
        return sum(
            MENU[item] * quantity for item, quantity in self.items.items()
        )

    def has_cookie(self) -> bool:
        """
        Returns True if cookie is present in order, otherwise returns False
        """
        return "cookie" in self.items

    def has_drink(self) -> bool:
        """
        Returns True if any beverage is present in order, otherwise returns False
        """
        drinks = ["americano", "espresso", "latte", "macchiato", "tea"]
        return any(item in drinks for item in self.items)


class CoffeeShopEvents:
    """
    Defines guest-employee inter-communication related events.

    """
    def __init__(self):
        self.guest_arrived = asyncio.Event()
        self.order_taken = asyncio.Event()
        self.coffee_ready = asyncio.Event()
        self.transaction_complete = asyncio.Event()

        self.guest_messages = asyncio.Queue()
        self.employee_messages = asyncio.Queue()

        self.shared_order = Order()


async def guest_task(events: CoffeeShopEvents, guest_script: List[str] = None):
    """Simulates a guest visiting the coffee shop"""

    if guest_script is None:
        guest_script = [
            "I'd like an americano.",
            "Yes, please.",
            "That's all."
        ]

    print("Guest: Arriving at coffee shop")
    await asyncio.sleep(2)  # Travel time

    events.guest_arrived.set()
    greeting = await events.employee_messages.get()
    print(f"Employee: {greeting}")

    for message in guest_script:
        print(f"Guest: {message}")
        await events.guest_messages.put(message)

        response = await events.employee_messages.get()
        print(f"Employee: {response}")

        if message.startswith("WAIT"):
            try:
                wait_time = int(message.split()[1])
                await asyncio.sleep(wait_time)
            except (IndexError, ValueError):
                pass

        if message.lower() == "that's all.":
            events.order_taken.set()
            await events.coffee_ready.wait()
            print("Guest: Received coffee, heading out")
            events.transaction_complete.set()
            break


async def employee_task(events: CoffeeShopEvents):
    """Simulates an employee handling customers"""

    print("Employee: Ready to assist")
    await asyncio.sleep(1)

    await events.guest_arrived.wait()

    await events.employee_messages.put(
        "Welcome to our coffee shop. What can I get you?")

    cookie_upsell_offered = False

    while not events.order_taken.is_set():
        try:
            message = await asyncio.wait_for(
                events.guest_messages.get(),
                timeout=5.0
            )

            if message.lower().startswith("i'd like"):
                item = message.lower().replace("i'd like ", ""). \
                    replace("a ", "").replace("an ", "").rstrip(".")
                events.shared_order.add_item(item)

                if not cookie_upsell_offered and \
                        events.shared_order.has_drink() and \
                        not events.shared_order.has_cookie():
                    cookie_upsell_offered = True
                    await events.employee_messages.put(
                        f"Would you like to add a cookie for ${MENU['cookie']}?")
                else:
                    await events.employee_messages.put("Would you like anything else?")

            elif message.lower().startswith("i don't want"):
                item = message.lower().replace("i don't want ", ""). \
                    replace("a ", "").replace("an ", "").rstrip(".")
                if events.shared_order.remove_item(item):
                    await events.employee_messages.put(
                        "Would you like anything else?")
                else:
                    await events.employee_messages.put("I don't understand.")

            elif message.lower() == "that's all.":
                total = events.shared_order.calculate_total()
                await events.employee_messages.put(f"Your total is ${total:.2f}. Thank you and have a nice day!")

            elif message.lower() == "yes, please.":
                if cookie_upsell_offered:
                    events.shared_order.add_item("cookie")
                    await events.employee_messages.put("Would you like anything else?")
                else:
                    await events.employee_messages.put("I don't understand.")

            elif message.lower() == "no, thank you.":
                if cookie_upsell_offered:
                    await events.employee_messages.put("Would you like anything else?")
                else:
                    await events.employee_messages.put("I don't understand.")

            elif message.lower() == "let me think.":
                await events.employee_messages.put("Please let me know when you are ready.")

            else:
                await events.employee_messages.put("I don't understand.")

        except asyncio.TimeoutError:
            pass

    print("Employee: Taking coffee order")
    await asyncio.sleep(2)

    print("Employee: Preparing coffee")
    await asyncio.sleep(2)

    print("Employee: Handing over coffee to guest")
    events.coffee_ready.set()

    await events.transaction_complete.wait()


async def main():
    events = CoffeeShopEvents()

    if len(sys.argv) > 1:
        with open(sys.argv[1], 'r', encoding='utf-8') as f:
            guest_script = [line.strip() for line in f.readlines()]
    else:
        guest_script = [
            "I'd like an americano.",
            "Yes, please.",
            "That's all."
        ]

    await asyncio.gather(
        guest_task(events, guest_script),
        employee_task(events)
    )

    print("\nOrder summary:")
    for item, quantity in events.shared_order.items.items():
        print(f"- {item}: {quantity} x ${MENU[item]:.2f}")
    print(f"Total: ${events.shared_order.calculate_total():.2f}")


if __name__ == "__main__":
    asyncio.run(main())
