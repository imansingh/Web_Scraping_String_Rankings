from scrapy import Spider, Request
from stringforum.items import StringforumItem

class StringforumSpider(Spider):
	name = 'stringforum_spider'
	allowed_urls = ['www.stringforum.net']
	# manually update range of below list comprehension. Range = # pages
	start_urls = ['https://www.stringforum.net/stringsearch.php?start=' + str(30*i) for i in range(79)]

	def parse(self, response):
		#extract review URLs from main ratings page
		url_list = response.xpath('//a[contains(@href,"ratings.php")]/@href').extract()
		first_reviews = ['https://www.stringforum.net/' + l + '&limit=1' for l in url_list]

		#send each review URL for further parsing
		for url in first_reviews:
			yield Request(url, callback=self.review_url_generator)

	def review_url_generator(self, response):
		num_ratings = int(response.xpath('//td[@valign="middle"]/text()').re(r'of\s+(.*)')[0])
		review_urls = [response.url + '&start=' + str(i) for i in range(num_ratings)]

		#send each review URL for further parsing
		for url in review_urls:
			yield Request(url, callback=self.parse_review)

	def parse_review (self, response):
		string_name = response.xpath('//td[@colspan="10"]//td[@class="xb"]//a[contains(@href,"sdnr")]/text()').extract_first()
		
		string_type_list = response.xpath('//td[@width="62"]/img/@alt').extract()
		try:
			string_type = ", ".join(string_type_list)
		except:
			string_type = "NA"

		price = response.xpath('//td[@colspan="10"]//td[@width="100"]/text()').extract_first()
		try:
			price = price[:-1]
		except:
			price = "Unkown"

		overall_durability = response.xpath('//table[@cellpadding="1"]//tr[3]//td[@class="b"][1]/img[@src = "grsq8.gif"]/@width').extract_first()
		overall_power = response.xpath('//table[@cellpadding="1"]//tr[3]//td[@class="b"][2]/img[@src = "grsq8.gif"]/@width').extract_first()
		overall_control = response.xpath('//table[@cellpadding="1"]//tr[3]//td[@class="b"][3]/img[@src = "grsq8.gif"]/@width').extract_first()
		overall_feel = response.xpath('//table[@cellpadding="1"]//tr[3]//td[@class="b"][4]/img[@src = "grsq8.gif"]/@width').extract_first()
		overall_comfort = response.xpath('//table[@cellpadding="1"]//tr[3]//td[@class="b"][5]/img[@src = "grsq8.gif"]/@width').extract_first()
		overall_spin = response.xpath('//table[@cellpadding="1"]//tr[3]//td[@class="b"][6]/img[@src = "grsq8.gif"]/@width').extract_first()
		overall_tension_stability = response.xpath('//table[@cellpadding="1"]//tr[3]//td[@class="b"][7]/img[@src = "grsq8.gif"]/@width').extract_first()
		overall_satisfaction = response.xpath('//table[@cellpadding="1"]//tr[3]//td[@class="b2"]//text()').extract_first()
		overall_rating = response.xpath('//table[@cellpadding="1"]//tr[3]//td[@class="b"][9]/img[@src = "grsq8.gif"]/@width').extract_first()
		num_ratings = int(response.xpath('//td[@valign="middle"]/text()').re(r'of\s+(.*)')[0])
		
		tester_name = response.xpath('//table[@cellpadding="4"]//tr[1]//td[@width="220"]/text()').extract_first()[:-2]
		try:
			tester_reviews = int(response.xpath('//table[@cellpadding="4"]//tr[1]//td[@class="y"]//a[contains(@href,"tester")]/text()').extract_first())
		except:
			tester_reviews = 1
		tester_style = response.xpath('//table[@cellpadding="4"]//tr[1]//td[@width="330"]/text()').extract_first()
		tester_tension = response.xpath('//table[@cellpadding="4"]//tr[3]//td[@colspan="3"]/text()').extract_first()
		tester_racquet = response.xpath('//table[@cellpadding="4"]//tr[3]//td[@colspan="4"]/text()').extract_first()

		durability_string = response.xpath('//table[@cellpadding="4"]//tr[5]//td[@class="yd"]').extract()[0]
		if durability_string.count('plus') > 0:
			durability = durability_string.count('plus')
		elif durability_string.count('normal') > 0:
			durability = 0
		elif durability_string.count('minus') > 0:
			durability = durability_string.count('minus') * -1
		else:
			durability = "NA"

		power_string = response.xpath('//table[@cellpadding="4"]//tr[5]//td[@class="yd"]').extract()[1]
		if power_string.count('plus') > 0:
			power = power_string.count('plus')
		elif power_string.count('normal') > 0:
			power = 0
		elif power_string.count('minus') > 0:
			power = power_string.count('minus') * -1
		else:
			power = "NA"

		control_string = response.xpath('//table[@cellpadding="4"]//tr[5]//td[@class="yd"]').extract()[2]
		if control_string.count('plus') > 0:
			control = control_string.count('plus')
		elif control_string.count('normal') > 0:
			control = 0
		elif control_string.count('minus') > 0:
			control = control_string.count('minus') * -1
		else:
			control = "NA"

		feel_string = response.xpath('//table[@cellpadding="4"]//tr[5]//td[@class="yd"]').extract()[3]
		if feel_string.count('plus') > 0:
			feel = feel_string.count('plus')
		elif feel_string.count('normal') > 0:
			feel = 0
		elif feel_string.count('minus') > 0:
			feel = feel_string.count('minus') * -1
		else:
			feel = "NA"

		comfort_string = response.xpath('//table[@cellpadding="4"]//tr[5]//td[@class="yd"]').extract()[4]
		if comfort_string.count('plus') > 0:
			comfort = comfort_string.count('plus')
		elif comfort_string.count('normal') > 0:
			comfort = 0
		elif comfort_string.count('minus') > 0:
			comfort = comfort_string.count('minus') * -1
		else:
			comfort = "NA"

		spin_string = response.xpath('//table[@cellpadding="4"]//tr[5]//td[@class="yd"]').extract()[5]
		if spin_string.count('plus') > 0:
			spin = spin_string.count('plus')
		elif spin_string.count('normal') > 0:
			spin = 0
		elif spin_string.count('minus') > 0:
			spin = spin_string.count('minus') * -1
		else:
			spin = "NA"

		tension_stability_string = response.xpath('//table[@cellpadding="4"]//tr[5]//td[@class="yd"]').extract()[6]
		if tension_stability_string.count('plus') > 0:
			tension_stability = tension_stability_string.count('plus')
		elif tension_stability_string.count('normal') > 0:
			tension_stability = 0
		elif tension_stability_string.count('minus') > 0:
			tension_stability = tension_stability_string.count('minus') * -1
		else:
			tension_stability = "NA"

		review_text_list = response.xpath('//table[@cellpadding="4"]//tr[6]//td[@colspan="7"]/text()').extract()
		try:
			review_text = "".join(review_text_list).replace("\r\n", " ")
		except:
			review_text = "NA"

		satisfied_string = response.xpath('//table[@cellpadding="4"]//tr[7]//td[@colspan="7"]').extract_first()
		if satisfied_string.count('smilie_good') > 0:
			tester_satisfaction = 1
		elif satisfied_string.count('smilie_bad') > 0:
			tester_satisfaction = -1
		elif satisfied_string.count('smilie_neutral') > 0:
			tester_satisfaction = 0
		else:
			tester_satisfaction = "NA"


		review_adjectives = response.xpath('//table[@cellpadding="4"]//tr[12]//td[@colspan="7"]/text()').extract_first()


		item = StringforumItem()
		item['string_name'] = string_name
		item['string_type'] = string_type
		item['price'] = price
		item['overall_durability'] = overall_durability
		item['overall_power'] = overall_power
		item['overall_control'] = overall_control
		item['overall_feel'] = overall_feel
		item['overall_comfort'] = overall_comfort
		item['overall_spin'] = overall_spin
		item['overall_tension_stability'] = overall_tension_stability
		item['overall_satisfaction'] = overall_satisfaction
		item['overall_rating'] = overall_rating
		item['num_ratings'] = num_ratings
		item['tester_name'] = tester_name
		item['tester_reviews'] = tester_reviews
		item['tester_style'] = tester_style
		item['tester_tension'] = tester_tension
		item['tester_racquet'] = tester_racquet
		item['durability'] = durability
		item['power'] = power
		item['control'] = control
		item['feel'] = feel
		item['comfort'] = comfort
		item['spin'] = spin
		item['tension_stability'] = tension_stability
		item['review_text'] = review_text
		item['tester_satisfaction'] = tester_satisfaction
		item['review_adjectives'] = review_adjectives
		yield item

   
    

 