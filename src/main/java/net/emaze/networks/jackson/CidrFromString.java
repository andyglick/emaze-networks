package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import java.io.IOException;
import net.emaze.networks.Cidr;

public class CidrFromString extends JsonDeserializer<Cidr> {

    @Override
    public Cidr deserialize(JsonParser jp, DeserializationContext dc) throws IOException, JsonProcessingException {
        return Cidr.parse(jp.getText());
    }
}
