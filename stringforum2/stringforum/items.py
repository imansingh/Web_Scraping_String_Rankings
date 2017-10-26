# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

from scrapy import Item, Field


class StringforumItem(Item):
    # define the fields for StringforumItem
    string_name = Field()
    string_type = Field()
    price = Field()
    overall_durability = Field()
    overall_power = Field()
    overall_control = Field()
    overall_feel = Field()
    overall_comfort = Field()
    overall_spin = Field()
    overall_tension_stability = Field()
    overall_satisfaction = Field()
    overall_rating = Field()
    num_ratings = Field()

    tester_name = Field()
    tester_reviews = Field()
    tester_style = Field()
    tester_tension = Field()
    tester_racquet = Field()
    durability = Field()
    power = Field()
    control = Field()
    feel = Field()
    comfort = Field()
    spin = Field()
    tension_stability = Field()
    review_text = Field()
    tester_satisfaction = Field()
    review_adjectives = Field()